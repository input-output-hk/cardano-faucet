{-# LANGUAGE NamedFieldPuns #-}

-- | Authentication/authorization for API keys, reCAPTCHA challenges, and rate-limit
-- checking.
module Cardano.Faucet.Web.Auth
  ( ApiKeyProtected,
    RecaptchaApi,
    SiteVerify,
    SiteVerifyRequest(..),
    recaptchaApi,
    siteVerify,
    runSiteVerify,
    checkRecaptcha,
    decideBetweenKeyAndCaptcha,
    checkRateLimits,
    insertUsage,
  ) where

import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime)
import Cardano.Faucet.Types (RateLimitAddress, ApiKey (..), RateLimitResult (..), SiteVerifyRequest(..), SiteVerifyReply (..), CaptchaToken, SecretKey, ForwardedFor, FaucetConfigFile (..), ApiKeyValue)
import Control.Concurrent.STM (TMVar, readTMVar, takeTMVar, putTMVar)
import Data.Map.Strict qualified as Map
import Cardano.Prelude
import Servant ((:>), ReqBody, FormUrlEncoded, Post, JSON, QueryParam', Optional, RemoteHost, Header)
import Servant.Client (ClientM, ClientError, client, BaseUrl (..), Scheme (..), mkClientEnv, runClientM)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
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
  TMVar (Map ApiKey (Map RateLimitAddress UTCTime)) ->
  NominalDiffTime ->
  STM RateLimitResult
checkRateLimits now addresses apikey limitState akvRateLimit = do
  mainMap <- readTMVar limitState
  let
    -- when many addresses where last used, for the current key
    apiKeyMap :: Map RateLimitAddress UTCTime
    apiKeyMap = fromMaybe mempty (Map.lookup apikey mainMap)
    -- when an address was last used
    getLastUsage :: RateLimitAddress -> Maybe UTCTime
    getLastUsage addr' = Map.lookup addr' apiKeyMap
    lastUsages :: [Maybe UTCTime]
    lastUsages = map getLastUsage addresses
    compareTimes :: Maybe UTCTime -> Maybe UTCTime -> Maybe UTCTime
    compareTimes Nothing Nothing = Nothing
    compareTimes (Just a) Nothing = Just a
    compareTimes Nothing (Just b) = Just b
    compareTimes (Just a) (Just b) = Just (max a b)
    -- when any of the addresses given, have last been used
    lastUsage :: Maybe UTCTime
    lastUsage = Cardano.Prelude.foldl' compareTimes Nothing lastUsages
    recordUsage :: STM ()
    recordUsage = do
      mapM_ (insertUsage limitState apikey now) addresses
  (allowed, result) <- case lastUsage of
    Nothing -> do
      -- this addr has never been used on this api key
      pure (True, RateLimitResultAllow)
    Just lastUsed -> do
      let after = addUTCTime akvRateLimit lastUsed
      pure $
        if now > after
          then (True, RateLimitResultAllow)
          else (False, RateLimitResultDeny $ after `diffUTCTime` now)
  when allowed recordUsage
  pure result

-- | Records a usage of the given API key at the given timestamp.
insertUsage ::
  TMVar (Map ApiKey (Map RateLimitAddress UTCTime)) -> ApiKey -> UTCTime -> RateLimitAddress -> STM ()
insertUsage tmvar apikey now addr = do
  mainMap <- takeTMVar tmvar
  let
    apiKeyMap :: Map RateLimitAddress UTCTime
    apiKeyMap = fromMaybe mempty (Map.lookup apikey mainMap)
    apiKeyMap' :: Map RateLimitAddress UTCTime
    apiKeyMap' = Map.insert addr now apiKeyMap
    mainMap' = Map.insert apikey apiKeyMap' mainMap
  putTMVar tmvar mainMap'
