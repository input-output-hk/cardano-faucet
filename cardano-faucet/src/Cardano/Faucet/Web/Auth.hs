{-# LANGUAGE NamedFieldPuns #-}

-- | Authentication/authorization for API keys, reCAPTCHA challenges, and rate-limit
-- checking.
module Cardano.Faucet.Web.Auth (
  ApiKeyProtected,
  RecaptchaApi,
  SiteVerify,
  SiteVerifyRequest (..),
  recaptchaApi,
  siteVerify,
  runSiteVerify,
  checkRecaptcha,
  decideBetweenKeyAndCaptcha,
  checkRateLimits,
) where

import Cardano.Faucet.Types (
  ApiKey (..),
  ApiKeyRateLimits (..),
  ApiKeyValue,
  CaptchaToken,
  FaucetConfigFile (..),
  ForwardedFor,
  RateLimit (..),
  RateLimitAddress,
  RateLimitResult (..),
  SecretKey,
  SiteVerifyReply (..),
  SiteVerifyRequest (..),
  UsageHistory (..),
 )
import Cardano.Prelude
import Control.Concurrent.STM (TMVar, putTMVar, takeTMVar)
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Servant (
  FormUrlEncoded,
  Header,
  JSON,
  Optional,
  Post,
  QueryParam',
  RemoteHost,
  ReqBody,
  (:>),
 )
import Servant.Client (
  BaseUrl (..),
  ClientError,
  ClientM,
  Scheme (..),
  client,
  mkClientEnv,
  runClientM,
 )
import System.IO (print)

-- create recaptcha api keys at https://www.google.com/recaptcha/admin
-- reCAPTCHA v2, "i am not a robot"

-- | reCAPTCHA Root API
type RecaptchaApi = SiteVerify

-- | Verify a user response to a Google reCAPTCHA challenge, with the legacy SiteVerify
-- method. See: https://developers.google.com/recaptcha/docs/verify
type SiteVerify =
  "recaptcha"
    :> "api"
    :> "siteverify"
    :> ReqBody '[FormUrlEncoded] SiteVerifyRequest
    :> Post '[JSON] SiteVerifyReply

-- | An authentication combinator that accepts either an API key or a Google reCAPTCHA
-- challenge response.
type ApiKeyProtected rest =
  QueryParam' '[Optional] "api_key" Text
    :> QueryParam' '[Optional] "g-recaptcha-response" CaptchaToken
    :> RemoteHost
    :> Header "X-Forwarded-For" ForwardedFor
    :> Header "Origin" Text
    :> rest

-- | Verify a user response to a Google reCAPTCHA challenge
recaptchaApi :: Proxy RecaptchaApi
recaptchaApi = Proxy

-- | Run a Google reCAPTCHA challenge in a Servant 'ClientM'
siteVerify :: SiteVerifyRequest -> ClientM SiteVerifyReply
siteVerify = client recaptchaApi

-- | Call the actual reCAPTCHA SiteVerify API endpoint
runSiteVerify :: SecretKey -> CaptchaToken -> IO (Either ClientError SiteVerifyReply)
runSiteVerify secret token = do
  manager' <- newTlsManagerWith tlsManagerSettings
  runClientM
    (siteVerify $ SiteVerifyRequest secret token Nothing)
    (mkClientEnv manager' (BaseUrl Https "www.google.com" 443 ""))

-- | Run a reCAPTCHA SiteVerify, then check the results. Returns true if the request ran
-- successfully and Google responded with success.
checkRecaptcha :: SecretKey -> CaptchaToken -> IO Bool
checkRecaptcha secret token = do
  res <- runSiteVerify secret token
  print res
  case res of
    (Left _err) -> do
      pure False
    (Right (SiteVerifyReply _ts _host)) -> do
      pure True
    _ -> do
      pure False

-- Authenticate a request using the given API key or reCAPTCHA. If the API key is valid,
-- it is returned along with its limits. If the API key is unspecified or invalid, it runs
-- a reCAPTCHA verification and returns its limits. If neither are valid, it returns
-- 'Nothing'
decideBetweenKeyAndCaptcha ::
  Maybe Text ->
  Maybe Text ->
  Maybe CaptchaToken ->
  FaucetConfigFile ->
  IO (Maybe (ApiKey, ApiKeyValue))
decideBetweenKeyAndCaptcha mType (Just apiKeyText) mToken config@FaucetConfigFile {fcfApiKeys} = case apiKeyText `Map.lookup` fcfApiKeys of
  -- api key was not valid, just use recaptcha
  -- TODO, should it give an error instead?
  Nothing -> decideBetweenKeyAndCaptcha mType Nothing mToken config
  Just limits -> pure $ Just (ApiKey apiKeyText, limits)
decideBetweenKeyAndCaptcha mType Nothing (Just token) FaucetConfigFile {fcfRecaptchaSecretKey, fcfRecaptchaLimits} = do
  let
    captchaType = fromMaybe "default" mType
  valid <- checkRecaptcha fcfRecaptchaSecretKey token
  case (valid, captchaType `Map.lookup` fcfRecaptchaLimits) of
    (True, Just limits) -> pure $ Just (Recaptcha captchaType, limits)
    _ -> pure Nothing
decideBetweenKeyAndCaptcha _ Nothing Nothing _ = pure Nothing

-- | Check rate limits for the given API key and returns whether the action is allowed. If so,
-- it records a usage of the key at the given timestamp.
checkRateLimits ::
  UTCTime ->
  [RateLimitAddress] ->
  ApiKey ->
  TMVar UsageHistory ->
  ApiKeyRateLimits ->
  STM RateLimitResult
checkRateLimits now addresses apikey limitState limits = do
  hist@(UsageHistory addrHist globalHist) <- takeTMVar limitState
  let
    -- When each address was used for the current key
    addrAccesses = fromMaybe mempty (Map.lookup apikey addrHist)
    -- When any address was used for the current key
    globalAccesses = fromMaybe mempty (Map.lookup apikey globalHist)
    -- Filter out global accesses outside of the configured window
    prunedGlobalHist = pruneGlobalAccesses limits now globalAccesses
    prunedHist = hist { uhGlobal = Map.insert apikey prunedGlobalHist globalHist } 

    result =
      checkAddrLimits now limits addresses addrAccesses
        <> checkGlobalLimits now limits prunedGlobalHist

  case result of
    RateLimitResultAllow -> do
      let hist' = recordUsage now apikey addresses prunedHist
      putTMVar limitState hist'
    RateLimitResultDeny _ ->
      -- Don't record usage if access was denied
      putTMVar limitState hist

  pure result

-- | Calculates whether a request is within per-address rate limit restrictions
checkAddrLimits ::
  UTCTime ->
  ApiKeyRateLimits ->
  [RateLimitAddress] ->
  Map RateLimitAddress UTCTime ->
  RateLimitResult
checkAddrLimits now limits addrs usageHist =
  case lastAccessedByAnyKey of
    Nothing -> RateLimitResultAllow -- Never been used for
    Just lastAccessed ->
      let nextAllowed = nextAccessAllowedAt lastAccessed
       in if now > nextAllowed
            then RateLimitResultAllow
            else RateLimitResultDeny (nextAllowed `diffUTCTime` now)
  where
    lastAccessedByAnyKey :: Maybe UTCTime
    lastAccessedByAnyKey = maximumMay . mapMaybe (`Map.lookup` usageHist) $ addrs

    nextAccessAllowedAt :: UTCTime -> UTCTime
    nextAccessAllowedAt = addUTCTime $ grlWindow (rlPerAddress limits)

pruneGlobalAccesses :: ApiKeyRateLimits -> UTCTime -> [UTCTime] -> [UTCTime]
pruneGlobalAccesses (ApiKeyRateLimits {rlGlobal}) now =
  filter $ isInWindow (grlWindow rlGlobal)
  where
    isInWindow :: NominalDiffTime -> UTCTime -> Bool
    isInWindow diffTime accessedAt = diffTime `addUTCTime` accessedAt > now

-- | Calculates whether a request is within global rate limit restrictions
checkGlobalLimits :: UTCTime -> ApiKeyRateLimits -> [UTCTime] -> RateLimitResult
checkGlobalLimits now limits@(ApiKeyRateLimits {rlGlobal}) usageHist =
  if length accessesInWindow < fromIntegral (grlLimit rlGlobal)
    then RateLimitResultAllow
    else RateLimitResultDeny (nextAccessAllowedAt `diffUTCTime` now)
  where
    accessesInWindow :: [UTCTime]
    accessesInWindow = pruneGlobalAccesses limits now usageHist

    nextAccessAllowedAt :: UTCTime
    nextAccessAllowedAt =
      maybe now (grlWindow rlGlobal `addUTCTime`) $
        lastMay $
          drop
            (fromIntegral (grlLimit rlGlobal) - 1)
            accessesInWindow

-- Records global and per-address usages for the specified addresses
recordUsage :: UTCTime -> ApiKey -> [RateLimitAddress] -> UsageHistory -> UsageHistory
recordUsage now key addrs (UsageHistory addrHist globalHist) =
  let addrHist' = foldr (recordAddrUsage now key) addrHist addrs
      globalHist' = recordGlobalUsage now key globalHist
   in UsageHistory
        { uhPerAddress = addrHist'
        , uhGlobal = globalHist'
        }

-- | Records a usage of an address at the given timestamp for an API key
recordAddrUsage ::
  UTCTime ->
  ApiKey ->
  RateLimitAddress ->
  Map ApiKey (Map RateLimitAddress UTCTime) ->
  Map ApiKey (Map RateLimitAddress UTCTime)
recordAddrUsage now key address addrHist =
  Map.insert key (mkUsage address now) addrHist
  where
    mkUsage :: RateLimitAddress -> UTCTime -> Map RateLimitAddress UTCTime
    mkUsage address' accessedAt =
      Map.insert address' accessedAt (lookupApiKey key addrHist)

    lookupApiKey key' = fromMaybe mempty . Map.lookup key'

-- | Records a global usage at the given timestamp for an API key
recordGlobalUsage :: UTCTime -> ApiKey -> Map ApiKey [UTCTime] -> Map ApiKey [UTCTime]
recordGlobalUsage now key = Map.insertWith (++) key [now]
