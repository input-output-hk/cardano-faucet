{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Faucet.Web (userAPI, server, SiteVerifyRequest (..)) where

import Cardano.Address.Derivation (Depth (PolicyK), XPrv)
import Cardano.Address.Style.Shelley (Shelley, getKey)
import Cardano.Api
import Cardano.Api.Experimental.Certificate (PoolId)
import Cardano.Api.Experimental.Certificate qualified as ExpCert
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L
import Cardano.CLI.Type.Common
import Cardano.Faucet.Misc (faucetValueToLovelace, parseAddress, stripMintingTokens, toFaucetValue)
import Cardano.Faucet.TxUtils (Fee (..), makeAndSignTx)
import Cardano.Faucet.Types (
  ApiKey (..),
  ApiKeyValue (..),
  CaptchaToken,
  DelegationReply (..),
  FaucetConfigFile (..),
  FaucetState (..),
  FaucetToken (FaucetMintToken, FaucetToken),
  FaucetValue (..),
  FaucetWebError (..),
  ForwardedFor (..),
  RateLimitAddress (..),
  RateLimitResult (..),
  SecretKey,
  SendMoneyReply (..),
  SendMoneySent (..),
  SiteKey (..),
  SiteVerifyReply (..),
  SiteVerifyRequest (..),
  UtxoStats (..),
  rootKeyToPolicyKey,
 )
import Cardano.Faucet.Utils (computeUtxoStats, findUtxoOfSize, prettyFriendlyTx)
import Cardano.Prelude hiding ((%))
import Control.Concurrent.STM (TMVar, putTMVar, readTMVar, takeTMVar, writeTQueue)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.IP (IPv6, fromHostAddress, ipv4ToIPv6)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Formatting (format, sformat, (%))
import Formatting.ShortFormatters hiding (b, f, l, x)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Media.MediaType ((//), (/:))
import Network.Socket (SockAddr (SockAddrInet))
import Protolude (print)
import Servant
import Servant.Client (
  BaseUrl (BaseUrl),
  ClientError,
  ClientM,
  Scheme (Https),
  client,
  mkClientEnv,
  runClientM,
 )

-- create recaptcha api keys at https://www.google.com/recaptcha/admin
-- reCAPTCHA v2, "i am not a robot"

instance FromHttpApiData PoolId where
  parseUrlPiece input = either (Left . T.pack) Right $ eitherDecode (LBS.fromStrict $ encodeUtf8 $ "\"" <> input <> "\"")

data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
  mimeRender _ = LT.encodeUtf8 . LT.fromStrict

type ApiKeyProtected rest =
  QueryParam' '[Optional] "api_key" Text
    :> QueryParam' '[Optional] "g-recaptcha-response" CaptchaToken
    :> RemoteHost
    :> Header "X-Forwarded-For" ForwardedFor
    :> Header "Origin" Text
    :> rest

type SendMoneyUrl =
  "send-money"
    :> Capture "destination_address" Text
    :> QueryParam' '[Optional] "type" Text
    :> ApiKeyProtected (Post '[JSON] (Headers '[Header "Access-Control-Allow-Origin" Text] SendMoneyReply))

type SendMoneyQuery =
  "send-money"
    :> QueryParam' '[Required] "address" Text
    :> QueryParam' '[Optional] "type" Text
    :> ApiKeyProtected (Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" Text] SendMoneyReply))

type Metrics = "metrics" :> Get '[PlainText] Text

type DelegateStakeUrl =
  "delegate"
    :> Capture "poolid" PoolId
    :> ApiKeyProtected (Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" Text] DelegationReply))

type DelegateStakeQuery =
  "delegate"
    :> QueryParam' '[Required] "poolid" PoolId
    :> ApiKeyProtected (Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" Text] DelegationReply))

type MintCoins =
  "mint-many"
    :> QueryParam' '[Required] "address" Text
    :> QueryParam' '[Required] "fee" Integer
    :> QueryParam' '[Required] "output_count" Integer
    :> QueryParam' '[Required] "tokens_per_utxo" Integer
    :> Get '[JSON] (Either FaucetWebError ())

type SiteVerify =
  "recaptcha"
    :> "api"
    :> "siteverify"
    :> ReqBody '[FormUrlEncoded] SiteVerifyRequest
    :> Post '[JSON] SiteVerifyReply

type GetSiteKey = "get-site-key" :> Get '[PlainText] Text

type GetBasicFaucet = "basic-faucet" :> Get '[HTML] Text

-- faucet root dir
type RootDir =
  SendMoneyUrl
    :<|> SendMoneyQuery
    :<|> Metrics
    :<|> DelegateStakeUrl
    :<|> DelegateStakeQuery
    :<|> GetSiteKey
    :<|> GetBasicFaucet
    :<|> MintCoins

-- recaptcha root dir
type CaptchaRootDir = SiteVerify

userAPI :: Proxy RootDir
userAPI = Proxy

recaptchaApi :: Proxy CaptchaRootDir
recaptchaApi = Proxy

siteVerify :: SiteVerifyRequest -> ClientM SiteVerifyReply
siteVerify = client recaptchaApi

doSiteVerify :: SecretKey -> CaptchaToken -> IO (Either ClientError SiteVerifyReply)
doSiteVerify secret token = do
  manager' <- newTlsManagerWith tlsManagerSettings
  runClientM
    (siteVerify $ SiteVerifyRequest secret token Nothing)
    (mkClientEnv manager' (BaseUrl Https "www.google.com" 443 ""))

server ::
  ShelleyBasedEra era ->
  FaucetState era ->
  Text ->
  Server RootDir
server era faucetState indexHtml =
  handleSendMoney era faucetState
    :<|> handleSendMoney era faucetState
    :<|> handleMetrics faucetState
    :<|> handleDelegateStake era faucetState
    :<|> handleDelegateStake era faucetState
    :<|> handleStaticFile site_key_reply
    :<|> handleStaticFile indexHtml
    :<|> handleMintCoins era faucetState
  where
    site_key_reply = sformat ("site_key = \"" % st % "\";") $ unSiteKey $ fcfRecaptchaSiteKey $ fsConfig faucetState

handleStaticFile :: Text -> Servant.Handler Text
handleStaticFile = pure

-- takes a TMVar of used and unused stake kets
-- atomically pops one off the unused list, adds it to the used list, and returns the popped key
getKeyToDelegate ::
  TMVar ([(Word32, SigningKey StakeExtendedKey, StakeCredential)], [(Word32, L.Coin, PoolId)]) ->
  PoolId ->
  STM (SigningKey StakeExtendedKey, StakeCredential)
getKeyToDelegate tmvar poolid = do
  (availableKeys, usedKeys) <- takeTMVar tmvar
  case (poolid `elem` map (\(_, _, p) -> p) usedKeys, availableKeys) of
    (True, _) -> do
      throwSTM FaucetWebErrorAlreadyDelegated
    (False, []) -> do
      throwSTM FaucetWebErrorStakeKeyNotFound
    (False, (index, skey, vkey) : rest) -> do
      putTMVar tmvar (rest, (index, 0, poolid) : usedKeys)
      pure (skey, vkey)

-- checks if a given origin is in the whitelist, and if so, puts that origin in the header
getCorsReply ::
  [Text] -> Maybe Text -> (a -> Headers '[Header "Access-Control-Allow-Origin" Text] a)
getCorsReply whitelist mOrigin = case mOrigin of
  Nothing -> noHeader
  (Just origin) -> if origin `elem` whitelist then addHeader origin else noHeader

handleMintCoins ::
  ShelleyBasedEra era ->
  FaucetState era ->
  Text ->
  Integer ->
  Integer ->
  Integer ->
  Servant.Handler (Either FaucetWebError ())
handleMintCoins era fs@FaucetState {fsTxQueue} addr fee output_count tokens_per_utxo = do
  liftIO $ runExceptT $ do
    addressAny <- parseAddress addr
    print addressAny
    (signedTx, txid) <-
      mintFreshTokens
        era
        fs
        0
        addressAny
        (UnsafeAssetName "Testtoken")
        tokens_per_utxo
        output_count
        (Fee $ L.Coin fee)
    let prettyTx = prettyFriendlyTx era signedTx
    liftIO $ atomically $ writeTQueue fsTxQueue (TxInMode era signedTx, prettyTx, txid)
    pure ()

{-test :: IO ()
test = do
  eResult <- runExceptT $ do
    config <- parseConfig "/home/clever/iohk/cardano-world/sample-config.json"
    rootK <- mnemonicToRootKey $ fcfMnemonic config
    fsUtxoTMVar <- liftIO $ newEmptyTMVarIO
    addressAny <- withExceptT (FaucetErrorTodo2 . show) $ parseAddress "addr_test1vq4p02j5rf5w69kldld0t9wt6fe2efvfhcxsk4qdhwm2pnglr5yaj"
    let
      addr :: AddressInEra undefined
      addr = anyAddressInShelleyBasedEra addressAny
      txid = fromMaybe undefined $ decode "8452cb2be57a8389f20e4c48bfb6e3ce9b7c2e655b9675f96025339af905c0c3"
      txout :: TxOut CtxUTxO era
      txout = TxOut addr _ _ _
      fakeUtxo :: Map TxIn (TxOut CtxUTxO era)
      fakeUtxo = Map.singleton (TxIn txid $ TxIx 0) txout
      fs = FaucetState
        { fsRootKey = rootK
        , fsUtxoTMVar = fsUtxoTMVar
        , fsStakeTMVar = Prelude.error "stake mvar"
        , fsNetwork = Testnet $ NetworkMagic 9
        , fsTxQueue = Prelude.error "tx queue"
        , fsPaymentSkey = Prelude.error "skey"
        , fsPaymentVkey = Prelude.error "vkey"
        , fsAcctKey = Prelude.error "acct key"
        , fsConfig = config
        , fsSendMoneyRateLimitState = undefined
        , fsDelegationRateLimitState = undefined
        , fsBucketSizes = Prelude.error "bucket size"
        , fsOwnAddress = Prelude.error "own addr"
        }
    liftIO $ atomically $ putTMVar fsUtxoTMVar fakeUtxo
    res <- withExceptT (FaucetErrorTodo2 . show) $ mintFreshTokens ShelleyEra fs 0x80000000 addressAny (AssetName "Testtoken") (Quantity 100)
    print res
    pure ()
  case eResult of
    Left err -> print $ renderFaucetError err
    Right res -> print res-}

txoutToValue :: TxOut CtxUTxO era -> Value
txoutToValue (TxOut _ txOutValue _ _) = txOutValueToValue txOutValue

data TokenState = TokenState
  { tsAssetId :: AssetId
  , tsPolicyId :: PolicyId
  , tsPolicySKey :: SigningKey PaymentExtendedKey
  , tsScript :: Script SimpleScript'
  }

getTokenState :: HasCallStack => Word32 -> AssetName -> FaucetState era -> TokenState
getTokenState policy_index name FaucetState {fsRootKey} = TokenState {tsAssetId, tsPolicyId, tsPolicySKey, tsScript}
  where
    policyKey :: Shelley 'PolicyK XPrv
    policyKey = rootKeyToPolicyKey fsRootKey (0x8000_0000 + policy_index)
    tsPolicySKey :: SigningKey PaymentExtendedKey
    tsPolicySKey = PaymentExtendedSigningKey $ getKey policyKey
    policy_vkey :: VerificationKey PaymentExtendedKey
    policy_vkey = getVerificationKey tsPolicySKey
    simpleScript :: SimpleScript
    simpleScript = RequireSignature $ verificationKeyHash $ castVerificationKey policy_vkey
    tsScript :: Script SimpleScript'
    tsScript = SimpleScript simpleScript
    tsPolicyId = scriptPolicyId tsScript
    tsAssetId = AssetId tsPolicyId name

-- | A mint entry passed to 'makeAndSignTx': the policy id, the assets to mint
-- under it, and the native (simple) minting policy serialised to CBOR. TxUtils
-- decodes the CBOR into the experimental SimpleScript for the target ledger era.
getOptionalMintOutput ::
  ShelleyBasedEra era ->
  FaucetState era ->
  FaucetValue ->
  ExceptT FaucetWebError IO (Value, [(PolicyId, PolicyAssets, ByteString)], [ShelleyWitnessSigningKey])
getOptionalMintOutput _ fs (FaucetValueMultiAsset _ (FaucetMintToken (policy_index, name, quant))) = do
  let TokenState {tsAssetId, tsPolicyId, tsPolicySKey, tsScript} = getTokenState policy_index name fs
  assetName <- case tsAssetId of
    AdaAssetId -> left $ FaucetWebErrorTodo "Not a multi asset"
    AssetId _ assetName -> pure assetName
  let
    assets = PolicyAssets $ Map.fromList [(assetName, quant)]
    valueToMint = valueFromList [(tsAssetId, quant)]
  pure
    ( valueToMint
    , [(tsPolicyId, assets, serialiseToCBOR tsScript)]
    , [WitnessPaymentExtendedKey tsPolicySKey]
    )
getOptionalMintOutput _ _ _ = pure (mempty, [], [])

mintFreshTokens ::
  ShelleyBasedEra era ->
  FaucetState era ->
  Word32 ->
  AddressAny ->
  AssetName ->
  Integer ->
  Integer ->
  Fee ->
  ExceptT FaucetWebError IO (Tx era, TxId)
mintFreshTokens sbe fs@FaucetState {fsUtxoTMVar, fsPaymentSkey, fsOwnAddress} policyIndex destinationAddress tokenname count tx_out_count (Fee feeLovelace) = do
  let TokenState {tsAssetId, tsPolicyId, tsPolicySKey, tsScript} = getTokenState policyIndex tokenname fs
  txinout@(_, txout) <- liftIO $ atomically $ findUtxoOfSize fsUtxoTMVar $ Ada $ L.Coin (1000 * 1_000_000)
  assetName <- case tsAssetId of
    AdaAssetId -> left $ FaucetWebErrorTodo "Not a multi asset"
    AssetId _ assetName -> pure assetName
  let
    quant = Quantity (count * tx_out_count)
    valueToMint = valueFromList [(tsAssetId, quant)]
    assets = PolicyAssets $ Map.fromList [(assetName, quant)]
    mintEntries = [(tsPolicyId, assets, serialiseToCBOR tsScript)]
    -- value in each utxo being created
    outputValue = valueFromList [(AdaAssetId, Quantity 10_000_000), (tsAssetId, Quantity count)]
    outputValues = replicate (fromIntegral tx_out_count) outputValue
    to_txout :: AddressAny -> Value -> TxOutAnyEra
    to_txout addr v = TxOutAnyEra addr v TxOutDatumByNone ReferenceScriptAnyEraNone
    feeValue = lovelaceToValue feeLovelace
    outputs = map (to_txout destinationAddress) outputValues
    output_sum = negateValue $ mconcat $ feeValue : outputValues
    input_sum = mconcat [txoutToValue txout, valueToMint]
    change = input_sum <> output_sum
    -- prepend the change if there is any
    outputsWithChange = if change == mempty then outputs else to_txout fsOwnAddress change : outputs
  makeAndSignTx
    sbe
    txinout
    (Right outputsWithChange)
    [fsPaymentSkey, WitnessPaymentExtendedKey tsPolicySKey]
    []
    mintEntries
    (Fee feeLovelace)

handleDelegateStake ::
  ShelleyBasedEra era ->
  FaucetState era ->
  PoolId ->
  Maybe Text ->
  Maybe CaptchaToken ->
  SockAddr ->
  Maybe ForwardedFor ->
  Maybe Text ->
  Servant.Handler (Headers '[Header "Access-Control-Allow-Origin" Text] DelegationReply)
handleDelegateStake
  sbe
  FaucetState
    { fsPaymentSkey
    , fsUtxoTMVar
    , fsTxQueue
    , fsStakeTMVar
    , fsConfig
    , fsDelegationRateLimitState
    , fsOwnAddress
    }
  poolId
  mApiKey
  mToken
  remoteip
  mForwardedFor
  mOrigin = do
    let
      clientIP = pickIp mForwardedFor remoteip
    eResult <- liftIO $ runExceptT $ do
      when (isNothing (fcfMaxStakeKeyIndex fsConfig)) $ left $ FaucetWebErrorTodo "delegation disabled"
      mReply <- liftIO $ decideBetweenKeyAndCaptcha Nothing mApiKey mToken fsConfig
      (key, limits) <- case mReply of
        Just (_, ApiKeyValue _ _ _ _ False) -> left FaucetWebErrorKeyCantDelegate
        Just x -> pure x
        Nothing -> left FaucetWebErrorInvalidApiKey
      now <- liftIO getCurrentTime
      res <- liftIO $ atomically $ do
        let
          maybeBumpLimitAndGetKey = do
            limitResult <-
              checkRateLimits
                now
                [RateLimitAddressPool poolId, RateLimitAddressNetwork clientIP]
                key
                fsDelegationRateLimitState
                limits
            case limitResult of
              RateLimitResultAllow -> do
                -- the rate limits allow the action at the current time
                -- get an unused stake key
                stakeKey <- getKeyToDelegate fsStakeTMVar poolId
                -- and get a txout to fund the delegation tx
                txinout <- findUtxoOfSize fsUtxoTMVar $ Ada $ L.Coin (fcfDelegationUtxoSize fsConfig * 1_000_000)
                pure $ Right (stakeKey, txinout)
              RateLimitResultDeny waitPeriod -> throwSTM $ FaucetWebErrorRateLimitExeeeded waitPeriod (serialiseToBech32 poolId)
        -- getKeyToDelegate and findUtxoOfSize can use throwSTM to report an error, and undo the entire atomic action
        catchSTM maybeBumpLimitAndGetKey $ pure . Left
      case res of
        Left err -> left err
        Right ((stake_skey, creds), txinout) -> do
          let
            poolKeyHash :: L.KeyHash L.StakePool = unStakePoolKeyHash poolId
            expCert =
              caseShelleyToBabbageOrConwayOrDijkstra
                ( \_ ->
                    let ledgerCert = L.mkDelegStakeTxCert (toShelleyStakeCredential creds) (unStakePoolKeyHash poolId)
                     in ExpCert.Certificate ledgerCert
                )
                ( \case
                    ConwayEraOnwardsConway ->
                      ExpCert.Certificate $ L.mkDelegTxCert (toShelleyStakeCredential creds) (L.DelegStake poolKeyHash)
                    ConwayEraOnwardsDijkstra ->
                      ExpCert.Certificate $ L.mkDelegTxCert (toShelleyStakeCredential creds) (L.DelegStake poolKeyHash)
                )
                sbe
            stake_witness = WitnessStakeExtendedKey stake_skey
          (signedTx, txid) <-
            makeAndSignTx
              sbe
              txinout
              (Left fsOwnAddress)
              [fsPaymentSkey, stake_witness]
              [(expCert, Exp.AnyKeyWitnessPlaceholder)]
              []
              (Fee $ L.Coin 200_000)
          let
            prettyTx = prettyFriendlyTx sbe signedTx
          putStrLn
            $ format
              (sh % ": delegating stake key to pool " % sh % " via txid " % sh)
              clientIP
              (serialiseToBech32 poolId)
              txid
          liftIO $ atomically $ writeTQueue fsTxQueue (TxInMode sbe signedTx, prettyTx, txid)
          pure $ DelegationReplySuccess txid
    let corsHeader = getCorsReply (fcfAllowedCorsOrigins fsConfig) mOrigin
    case eResult of
      Left err -> do
        pure $ corsHeader $ DelegationReplyError err
      Right result -> do
        pure $ corsHeader result

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

-- check the rate limits for the given key, update the tmvar to record the usage of this key, and return if the action is allowed or not
checkRateLimits ::
  UTCTime ->
  [RateLimitAddress] ->
  ApiKey ->
  TMVar (Map ApiKey (Map RateLimitAddress UTCTime)) ->
  ApiKeyValue ->
  STM RateLimitResult
checkRateLimits now addresses apikey limitState ApiKeyValue {akvRateLimit} = do
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
      pure
        $ if now > after
          then (True, RateLimitResultAllow)
          else (False, RateLimitResultDeny $ after `diffUTCTime` now)
  when allowed recordUsage
  pure result

checkRecaptcha :: SecretKey -> CaptchaToken -> IO Bool
checkRecaptcha secret token = do
  res <- doSiteVerify secret token
  print res
  case res of
    (Left _err) -> do
      pure False
    (Right (SiteVerifyReply _ts _host)) -> do
      pure True
    _ -> do
      pure False

data MetricValue = MetricValueInt Integer | MetricValueFloat Float | MetricValueStr Text
  deriving (Show)

valToString :: MetricValue -> Text
valToString (MetricValueInt i) = show i
valToString (MetricValueFloat f) = show f
valToString (MetricValueStr str) = str

data Metric = Metric (Map Text MetricValue) Text MetricValue deriving (Show)

attributesToString :: Map Text MetricValue -> Text
attributesToString map' = if Map.null map' then "" else wrapped
  where
    wrapped = "{" <> joinedAttrs <> "}"
    joinedAttrs =
      T.intercalate ","
        $ Map.elems
        $ Map.mapWithKey (\key val -> key <> "=\"" <> valToString val <> "\"") map'

toMetric :: Metric -> Text
toMetric (Metric attribs key val) = key <> attributesToString attribs <> " " <> valToString val

handleMetrics :: FaucetState era -> Servant.Handler Text
handleMetrics FaucetState {fsUtxoTMVar, fsBucketSizes, fsConfig, fsStakeTMVar} = do
  liftIO $ do
    (utxo, (stakeUnused, stakeUsed)) <- atomically $ do
      u <- readTMVar fsUtxoTMVar
      stake <- readTMVar fsStakeTMVar
      pure (u, stake)
    let
      -- how many utxo exist at each value
      stats :: Map FaucetValue Int
      (UtxoStats stats) = computeUtxoStats utxo
      -- utxo that are entirely missing but required by something
      missingUtxo :: Map FaucetValue Int
      missingUtxo = Map.difference (Map.fromList $ map (\fv -> (fv, 0)) fsBucketSizes) stats
      isRequiredSize :: FaucetValue -> Maybe (Text, MetricValue)
      isRequiredSize v = if v `elem` fsBucketSizes then Just ("is_valid", MetricValueInt 1) else Nothing
      isForDelegation v =
        if v == L.Coin (fcfDelegationUtxoSize fsConfig * 1_000_000)
          then Just ("for_delegation", MetricValueInt 1)
          else Nothing
      valueAttribute :: FaucetValue -> [Maybe (Text, MetricValue)]
      valueAttribute fv =
        [Just ("lovelace", MetricValueInt l), Just ("ada", MetricValueFloat $ fromIntegral l / 1_000_000)]
        where
          L.Coin l = faucetValueToLovelace fv
      tokenAttributes :: FaucetToken -> [Maybe (Text, MetricValue)]
      tokenAttributes (FaucetToken (AssetId (PolicyId scripthash) (UnsafeAssetName _tokenname), _quant)) =
        [ Just ("policyid", MetricValueStr $ serialiseToRawBytesHexText scripthash)
        -- has escaping issues, T_1\n breaks prometheus
        -- , Just ("tokenname", MetricValueStr $ T.decodeLatin1 tokenname)
        ]
      tokenAttributes _ = []
      toStats :: FaucetValue -> Int -> Metric
      toStats fv@((Ada l)) count =
        Metric
          (Map.fromList $ catMaybes $ valueAttribute fv <> [isRequiredSize fv, isForDelegation l])
          "faucet_utxo"
          (MetricValueInt $ fromIntegral count)
      toStats fv@(FaucetValueMultiAsset _ll token) count =
        Metric
          (Map.fromList $ catMaybes $ valueAttribute fv <> [isRequiredSize fv] <> tokenAttributes token)
          "faucet_utxo"
          (MetricValueInt $ fromIntegral count)
      toStats fv@(FaucetValueManyTokens _) count =
        Metric
          (Map.fromList $ catMaybes $ valueAttribute fv)
          "faucet_utxo_too_many_tokens"
          (MetricValueInt $ fromIntegral count)
      stakeUnusedToMetric :: Metric
      stakeUnusedToMetric = Metric mempty "faucet_delegation_available" (MetricValueInt $ fromIntegral $ length stakeUnused)
      stakeUsedToMetric :: Metric
      stakeUsedToMetric = Metric mempty "faucet_delegation_pools" (MetricValueInt $ fromIntegral $ length stakeUsed)
      stakeRewardsMetric :: [Metric]
      stakeRewardsMetric =
        map
          ( \(index, L.Coin reward, pool) ->
              Metric
                ( Map.fromList
                    [("index", MetricValueInt $ fromIntegral index), ("pool", MetricValueStr $ serialiseToBech32 pool)]
                )
                "faucet_delegation_rewards"
                (MetricValueInt reward)
          )
          stakeUsed
      utxoMetrics :: [Metric]
      utxoMetrics = Map.foldlWithKey (\rows value count -> toStats value count : rows) [] (stats <> missingUtxo)
      metrics :: [Metric]
      metrics = utxoMetrics <> [stakeUnusedToMetric, stakeUsedToMetric] <> stakeRewardsMetric
      result = Cardano.Prelude.unlines $ Cardano.Prelude.map toMetric metrics
    pure result

pickIp :: Maybe ForwardedFor -> SockAddr -> IPv6
pickIp (Just (ForwardedFor (a : _))) _ = a
pickIp _ (SockAddrInet _port hostaddr) = ipv4ToIPv6 $ fromHostAddress hostaddr
pickIp _ _ = ipv4ToIPv6 $ fromHostAddress 0x100_007f -- 127.0.0.1, little-endian arch

-- if a valid api key is given, return that key and its limits
-- if the apikey is invalid, act like it didnt exist
-- if a recaptcha token exists and is valid, return those limits
-- if all keys are missing or invalid return Nothing
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

handleSendMoney ::
  ShelleyBasedEra era ->
  FaucetState era ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe CaptchaToken ->
  SockAddr ->
  Maybe ForwardedFor ->
  Maybe Text ->
  Servant.Handler (Headers '[Header "Access-Control-Allow-Origin" Text] SendMoneyReply)
handleSendMoney sbe fs@FaucetState {fsUtxoTMVar, fsPaymentSkey, fsTxQueue, fsConfig, fsSendMoneyRateLimitState} addr mType mApiKey mToken remoteip mForwardedFor mOrigin = do
  let clientIP = pickIp mForwardedFor remoteip
  eResult <- liftIO $ runExceptT $ do
    addressAny <- parseAddress addr
    mReply <- liftIO $ decideBetweenKeyAndCaptcha mType mApiKey mToken fsConfig
    (key, limits) <- case mReply of
      Just x -> pure x
      Nothing -> left FaucetWebErrorInvalidApiKey
    let
      limitFaucetValue = toFaucetValue limits
      withoutMintedTokens = stripMintingTokens limitFaucetValue
    print limits
    print limitFaucetValue
    print withoutMintedTokens
    now <- liftIO getCurrentTime
    result <- liftIO $ atomically $ do
      let
        maybeBumpLimitAndGetUtxo = do
          limitResult <-
            checkRateLimits
              now
              [RateLimitAddressCardano addressAny, RateLimitAddressNetwork clientIP]
              key
              fsSendMoneyRateLimitState
              limits
          case limitResult of
            RateLimitResultAllow -> do
              txinout <- findUtxoOfSize fsUtxoTMVar withoutMintedTokens
              pure $ Right txinout
            RateLimitResultDeny waitPeriod -> throwSTM $ FaucetWebErrorRateLimitExeeeded waitPeriod (serialiseAddress addressAny)
      -- findUtxoOfSize can use throwSTM to report an error, and undo the entire atomic action
      catchSTM maybeBumpLimitAndGetUtxo $ pure . Left
    txinout@(txin, txout) <- case result of
      Left err -> left err
      Right txinout -> pure txinout
    (mintedValue, mintField, extraKeys) <- getOptionalMintOutput sbe fs limitFaucetValue
    let
      feeLovelace = L.Coin 200_000
      txInValue = txoutToValue txout
      valueUserShouldReceive = txInValue <> mintedValue <> negateValue (lovelaceToValue feeLovelace)
      outputs :: [TxOutAnyEra]
      outputs = [TxOutAnyEra addressAny valueUserShouldReceive TxOutDatumByNone ReferenceScriptAnyEraNone]
    (signedTx, txid) <-
      makeAndSignTx
        sbe
        txinout
        (Right outputs)
        (extraKeys <> [fsPaymentSkey])
        []
        mintField
        (Fee feeLovelace)
    putStrLn $ format ("txin is worth: " % sh) txInValue
    putStrLn $ format ("user should receive: " % sh) valueUserShouldReceive
    putStrLn
      $ format
        (sh % ": sending funds to address " % st % " via txid " % sh)
        clientIP
        (serialiseAddress addressAny)
        txid
    let
      prettyTx = prettyFriendlyTx sbe signedTx
    liftIO $ atomically $ writeTQueue fsTxQueue (TxInMode sbe signedTx, prettyTx, txid)
    return $ SendMoneyReplySuccess $ SendMoneySent txid txin limitFaucetValue
  let corsHeader = getCorsReply (fcfAllowedCorsOrigins fsConfig) mOrigin
  case eResult of
    Right msg -> pure $ corsHeader msg
    Left err -> do
      liftIO $ logError clientIP err
      pure $ corsHeader $ SendMoneyError err

logError :: IPv6 -> FaucetWebError -> IO ()
logError ip (FaucetWebErrorRateLimitExeeeded secs addr) =
  putStrLn
    $ format (sh % ": rate limit exeeded for " % t % " will reset in " % sh) ip (LT.fromStrict addr) secs
logError ip (FaucetWebErrorInvalidAddress addr _) = putStrLn $ format (sh % ": invalid cardano address: " % t) ip (LT.fromStrict addr)
logError ip FaucetWebErrorInvalidApiKey = putStrLn $ format (sh % ": invalid api key") ip
logError ip (FaucetWebErrorUtxoNotFound value) = putStrLn $ format (sh % ": faucet out of funds for: " % sh) ip value
logError _ FaucetWebErrorEraConversion = putStr @Text "era conversion error"
logError ip err = putStrLn $ format (sh % ": unsupported error: " % sh) ip err
