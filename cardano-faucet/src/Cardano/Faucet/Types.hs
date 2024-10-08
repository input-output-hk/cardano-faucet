{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet.Types (
  FaucetValue (..),
  ApiKeyValue (..),
  FaucetToken (..),
  FaucetWebError (..),
  UtxoStats (..),
  CaptchaToken,
  ForwardedFor (..),
  SendMoneyReply (..),
  DelegationReply (..),
  SiteVerifyReply (..),
  SiteVerifyRequest (..),
  SecretKey,
  FaucetState (..),
  RateLimitResult (..),
  ApiKey (..),
  RateLimitAddress (..),
  FaucetConfigFile (..),
  SiteKey (..),
  SendMoneySent (..),
  rootKeyToPolicyKey,
  FaucetError (..),
  StakeKeyIntermediateState (..),
  StakeKeyState (..),
  accountKeyToStakeKey,
  parseConfig,
  mnemonicToRootKey,
  rootKeytoAcctKey,
  accountKeyToPaymentKey,
  renderFaucetError,
  test,
) where

import Cardano.Address.Derivation (
  Depth (AccountK, PaymentK, PolicyK, RootK),
  DerivationType (Hardened, Soft),
  Index,
  XPrv,
  deriveAccountPrivateKey,
  deriveAddressPrivateKey,
  genMasterKeyFromMnemonic,
  indexFromWord32,
 )
import Cardano.Address.Style.Shelley (Role (Stake, UTxOExternal), Shelley, derivePolicyPrivateKey)
import Cardano.Api (
  AddressAny,
  AnyCardanoEra,
  AssetId (AdaAssetId, AssetId),
  CtxUTxO,
  FileError,
  HashableScriptData,
  InputDecodeError,
  PaymentExtendedKey,
  Quantity,
  SigningKey,
  TxId,
  TxIn,
  TxInMode,
  TxOut,
  VerificationKey,
  docToText,
 )
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (
  AssetName (..),
  NetworkId (Mainnet, Testnet),
  NetworkMagic (NetworkMagic),
  PoolId,
  ShelleyWitnessSigningKey,
  StakeCredential,
  StakeExtendedKey,
 )
import Cardano.CLI.Types.Errors.AddressCmdError
import Cardano.Mnemonic (getMkSomeMnemonicError, mkSomeMnemonic)
import Cardano.Prelude
import Control.Concurrent.STM (TMVar, TQueue)
import Control.Monad.Trans.Except.Extra (left)
import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap (member)
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 qualified as BSC
import Data.Either.Combinators (mapRight)
import Data.IP (IP (..), IPv6, ipv4ToIPv6)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Protolude (print)
import Servant (FromHttpApiData (parseHeader, parseQueryParam, parseUrlPiece))
import Web.Internal.FormUrlEncoded (ToForm (toForm), fromEntriesByKey)
import Prelude (String, error, fail, read)

-- the sitekey, secretkey, and token from recaptcha
newtype SiteKey = SiteKey {unSiteKey :: Text} deriving (Show)
newtype SecretKey = SecretKey {unSecretKey :: Text} deriving (Show)
newtype CaptchaToken = CaptchaToken Text

instance FromHttpApiData CaptchaToken where
  parseHeader bs = mapRight CaptchaToken (parseHeader bs)
  parseQueryParam t = mapRight CaptchaToken (parseQueryParam t)

-- the X-Forwarded-For header
newtype ForwardedFor = ForwardedFor [IPv6] deriving (Eq, Show)

parseIp :: IP -> IPv6
parseIp (IPv4 ip) = ipv4ToIPv6 ip
parseIp (IPv6 ip) = ip

parseIpList :: Prelude.String -> ForwardedFor
parseIpList input =
  ForwardedFor $ reverse $ map (\addr -> parseIp (Prelude.read addr :: IP)) (splitOn "," input)

instance FromHttpApiData ForwardedFor where
  parseHeader = Right . parseIpList . BSC.unpack
  parseUrlPiece = Right . parseIpList . T.unpack

-- errors not sent to users
data FaucetError
  = FaucetErrorSocketNotFound
  | FaucetErrorLoadingKey (FileError InputDecodeError)
  | FaucetErrorParsingConfig String
  | FaucetErrorConfigFileNotSet
  | FaucetErrorBadMnemonic Text
  | FaucetErrorBadIdx
  | FaucetErrorAddr AddressCmdError
  | FaucetErrorTodo2 Text
  deriving (Generic)

renderFaucetError :: FaucetError -> Text
renderFaucetError FaucetErrorSocketNotFound = "socket not found"
renderFaucetError (FaucetErrorLoadingKey err) = show err
renderFaucetError (FaucetErrorParsingConfig err) = show err
renderFaucetError FaucetErrorConfigFileNotSet = "$CONFIG_FILE not set"
renderFaucetError (FaucetErrorBadMnemonic msg) = "bad mnemonic " <> msg
renderFaucetError FaucetErrorBadIdx = "bad index"
renderFaucetError (FaucetErrorAddr err) = docToText $ renderAddressCmdError err
renderFaucetError (FaucetErrorTodo2 err) = show err

-- errors that can be sent to the user
data FaucetWebError
  = FaucetWebErrorInvalidAddress Text Text
  | FaucetWebErrorInvalidApiKey
  | FaucetWebErrorKeyCantDelegate
  | FaucetWebErrorRateLimitExeeeded NominalDiffTime Text
  | FaucetWebErrorUtxoNotFound FaucetValue
  | FaucetWebErrorEraConversion
  | FaucetWebErrorTodo Text
  | FaucetWebErrorFeatureMismatch AnyCardanoEra
  | FaucetWebErrorConsensusModeMismatchTxBalance Text AnyCardanoEra
  | FaucetWebErrorEraMismatch Text
  | FaucetWebErrorAutoBalance Text
  | FaucetWebErrorBadIdx
  | FaucetWebErrorAlreadyDelegated
  | FaucetWebErrorStakeKeyNotFound
  deriving (Generic, Show)

instance Exception FaucetWebError

instance Aeson.ToJSON FaucetWebError where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

-- an api key
-- Recaptcha is a special key, that can only be obtained by answering a recaptcha prompt, and has an optional type=x in the URL
data ApiKey = Recaptcha Text | ApiKey Text deriving (Ord, Eq)

-- the state of the entire faucet
data FaucetState era = FaucetState
  { fsUtxoTMVar :: TMVar (Map TxIn (TxOut CtxUTxO era))
  , fsStakeTMVar ::
      TMVar ([(Word32, SigningKey StakeExtendedKey, StakeCredential)], [(Word32, L.Coin, PoolId)])
  , fsNetwork :: NetworkId
  , fsTxQueue :: TQueue (TxInMode, ByteString)
  , fsRootKey :: Shelley 'RootK XPrv
  , fsPaymentSkey :: ShelleyWitnessSigningKey
  , fsPaymentVkey :: VerificationKey PaymentExtendedKey
  , fsAcctKey :: Shelley 'AccountK XPrv
  , fsConfig :: FaucetConfigFile
  , fsSendMoneyRateLimitState :: TMVar (Map ApiKey (Map RateLimitAddress UTCTime))
  , fsDelegationRateLimitState :: TMVar (Map ApiKey (Map RateLimitAddress UTCTime))
  , fsBucketSizes :: [FaucetValue]
  , fsOwnAddress :: AddressAny
  }

data ScriptDataOrFile
  = -- | By reference to a CBOR file
    ScriptDataCborFile FilePath
  | -- | By reference to a JSON file
    ScriptDataJsonFile FilePath
  | -- | By value
    ScriptDataValue HashableScriptData
  deriving (Eq, Show)

-- the result of checking a rate limit
data RateLimitResult = RateLimitResultAllow | RateLimitResultDeny NominalDiffTime

-- the key for rate limits
data RateLimitAddress
  = RateLimitAddressCardano AddressAny
  | RateLimitAddressNetwork IPv6
  | RateLimitAddressPool PoolId
  deriving (Eq, Ord)

-- send-money success reply
data SendMoneySent = SendMoneySent
  { txid :: TxId
  , txin :: TxIn
  , amount :: FaucetValue
  }

data StakeKeyIntermediateState
  = StakeKeyIntermediateStateNotRegistered Word32
  | StakeKeyIntermediateStateRegistered (Word32, SigningKey StakeExtendedKey, StakeCredential, L.Coin)

data StakeKeyState
  = StakeKeyRegistered Word32 (SigningKey StakeExtendedKey) StakeCredential L.Coin
  | StakeKeyDelegated Word32 L.Coin PoolId
  | StakeKeyNotRegistered Word32
  deriving (Show)

-- the full reply type for /send-money
data SendMoneyReply
  = SendMoneyReplySuccess SendMoneySent
  | SendMoneyError FaucetWebError

instance Aeson.ToJSON SendMoneyReply where
  toJSON (SendMoneyReplySuccess (SendMoneySent {txid, txin, amount})) = Aeson.object ["txid" .= txid, "txin" .= txin, "amount" .= amount]
  toJSON (SendMoneyError err) = Aeson.object ["error" .= err]

-- the full reply type for /delegate
data DelegationReply
  = DelegationReplySuccess TxId
  | DelegationReplyError FaucetWebError

instance Aeson.ToJSON DelegationReply where
  toJSON (DelegationReplySuccess txid) = Aeson.object ["success" .= True, "txid" .= txid]
  toJSON (DelegationReplyError err) = Aeson.object ["error" .= err]

-- a complete description of an api key
data ApiKeyValue = ApiKeyValue
  { akvApiKey :: Text
  , akvLovelace :: L.Coin
  , akvRateLimit :: NominalDiffTime
  , akvTokens :: Maybe FaucetToken
  , akvCanDelegate :: Bool
  }
  deriving (Generic, Show)

instance Aeson.FromJSON ApiKeyValue where
  parseJSON = Aeson.withObject "ApiKeyValue" $ \v -> do
    akvApiKey <- v .: "api_key"
    akvLovelace <- v .: "lovelace"
    akvRateLimit <- v .: "rate_limit"
    akvTokens <- v .:? "tokens"
    akvCanDelegate <- fromMaybe False <$> v .:? "delegate"
    pure $ ApiKeyValue {..}

-- the complete config file
data FaucetConfigFile = FaucetConfigFile
  { fcfMnemonic :: [Text]
  , fcfApiKeys :: Map Text ApiKeyValue
  , fcfRecaptchaLimits :: Map Text ApiKeyValue
  , fcfNetwork :: NetworkId
  , fcfMaxStakeKeyIndex :: Maybe Word32
  , fcfDebug :: Bool
  , fcfDelegationUtxoSize :: Integer
  , fcfRecaptchaSiteKey :: SiteKey
  , fcfRecaptchaSecretKey :: SecretKey
  , fcfAllowedCorsOrigins :: [Text]
  , fcfAddressIndex :: Word32
  }
  deriving (Generic, Show)

-- copied from bench/tx-generator/src/Cardano/TxGenerator/Internal/Orphans.hs
instance Aeson.FromJSON NetworkId where
  parseJSON j = case j of
    Aeson.String "Mainnet" -> pure Mainnet
    Aeson.Object v -> v .:? "Testnet" >>= maybe failure (pure . Testnet . NetworkMagic)
    _ -> failure
    where
      failure = fail $ "could not parse NetworkId: " <> show j

instance Aeson.FromJSON FaucetConfigFile where
  parseJSON = Aeson.withObject "FaucetConfigFile" $ \o -> do
    fcfMnemonic <- o .: "mnemonic"
    apiKeyList <- o .: "api_keys"
    let fcfApiKeys = Map.fromList $ map (\key@ApiKeyValue {akvApiKey} -> (akvApiKey, key)) apiKeyList
    fcfRecaptchaLimits <- o .: "recaptcha_limits"
    fcfNetwork <- o .: "network"
    fcfMaxStakeKeyIndex <- o .: "max_stake_key_index"
    fcfDebug <- o .: "debug"
    fcfDelegationUtxoSize <- o .: "delegation_utxo_size"
    fcfRecaptchaSiteKey <- SiteKey <$> o .: "recaptcha_site_key"
    fcfRecaptchaSecretKey <- SecretKey <$> o .: "recaptcha_secret_key"
    fcfAllowedCorsOrigins <- o .: "allowed_cors_origins"
    fcfAddressIndex <- fromMaybe 0 <$> o .:? "address_index"
    pure FaucetConfigFile {..}

-- a value with only ada, or a value containing a mix of assets
-- TODO, maybe replace with the cardano Value type?
data FaucetValue
  = Ada L.Coin
  | FaucetValueMultiAsset L.Coin FaucetToken
  | FaucetValueManyTokens L.Coin
  deriving (Show, Eq, Ord)

-- tokenToValue :: FaucetToken -> Value
-- tokenToValue (FaucetToken (AssetId policyid token, q)) = object [ "policyid" .= policyid, "token" .= token, "quantity" .= q ]
-- tokenToValue (FaucetToken (AdaAssetId, q)) = object [ "lovelace" .= q ]

instance Aeson.ToJSON FaucetValue where
  toJSON (Ada lovelace) = Aeson.object ["lovelace" .= lovelace]
  toJSON (FaucetValueMultiAsset _ _) = Aeson.String "TODO"
  toJSON (FaucetValueManyTokens _) = Aeson.String "unsupported"

data UtxoStats = UtxoStats (Map FaucetValue Int) deriving (Show)

-- a request to recaptcha
data SiteVerifyRequest = SiteVerifyRequest
  { svrSecret :: SecretKey
  , svrResponse :: CaptchaToken
  , svrRemoteIP :: Maybe Text
  }

instance ToForm SiteVerifyRequest where
  toForm (SiteVerifyRequest (SecretKey secret) (CaptchaToken token) mRemoteIp) = fromEntriesByKey foo
    where
      foo :: [(Text, [Text])]
      foo = [("secret", [secret]), ("response", [token])] ++ maybe [] (\x -> [("remoteip", [x])]) mRemoteIp

-- a reply from recaptcha
data SiteVerifyReply
  = SiteVerifyReply Text Text
  | SiteVerifyError [Text]
  deriving (Generic, Show)

-- example replies:
-- { "success": false, "error-codes": [ "timeout-or-duplicate" ]}
instance Aeson.FromJSON SiteVerifyReply where
  parseJSON = Aeson.withObject "SiteVerifyReply" $ \o -> do
    success <- o .: "success"
    case success of
      True -> do
        ts <- o .: "challenge_ts"
        hostname <- o .: "hostname"
        pure $ SiteVerifyReply ts hostname
      False -> do
        errors <- o .: "error-codes"
        pure $ SiteVerifyError errors

data FaucetToken = FaucetToken (AssetId, Quantity) | FaucetMintToken (Word32, AssetName, Quantity)
  deriving (Show, Eq, Ord)

parseToken :: Aeson.Object -> Parser AssetName
parseToken v = do
  mToken <- v .:? "token"
  case mToken of
    Just t -> pure $ AssetName $ encodeUtf8 t
    Nothing -> v .: "tokenHex"

instance Aeson.FromJSON FaucetToken where
  parseJSON = Aeson.withObject "FaucetToken" $ \v -> do
    case ("policy_id" `member` v) of
      True -> do
        policyid <- v .: "policy_id"
        quantity <- v .: "quantity"
        token <- parseToken v
        pure $ FaucetToken (AssetId policyid token, quantity)
      False -> do
        policy_index <- v .: "policy_index"
        token <- parseToken v
        quantity <- v .: "quantity"
        pure $ FaucetMintToken (policy_index, token, quantity)

instance Aeson.ToJSON FaucetToken where
  toJSON (FaucetToken (AssetId policyid token, quant)) = Aeson.object ["policy_id" .= policyid, "quantity" .= quant, "token" .= token]
  toJSON (FaucetToken (AdaAssetId, quant)) = Aeson.object ["assetid" .= ("ada" :: Text), "quantity" .= quant]
  toJSON (FaucetMintToken (_policyid, _token, _quant)) = Aeson.String "TODO"

parseConfig :: FilePath -> ExceptT FaucetError IO FaucetConfigFile
parseConfig path = do
  eResult <- liftIO $ Aeson.eitherDecodeFileStrict path
  case eResult of
    Left err -> left $ FaucetErrorParsingConfig err
    Right res -> pure res

mnemonicToRootKey :: Monad m => [Text] -> ExceptT FaucetError m (Shelley 'RootK XPrv)
mnemonicToRootKey mnemonic = do
  mw <-
    either (left . FaucetErrorBadMnemonic . T.pack . getMkSomeMnemonicError) pure
      $ mkSomeMnemonic @'[24] mnemonic
  pure $ genMasterKeyFromMnemonic mw mempty

convertHardenedIndex :: HasCallStack => Word32 -> Index 'Hardened depth
convertHardenedIndex idx = fromMaybe (error "bad hardened index") $ indexFromWord32 idx

convertSoftIndex :: Word32 -> Index 'Soft depth
convertSoftIndex idx = fromMaybe (error "bad soft index") $ indexFromWord32 idx

rootKeytoAcctKey :: Shelley 'RootK XPrv -> Word32 -> Shelley 'AccountK XPrv
rootKeytoAcctKey rootK index = deriveAccountPrivateKey rootK $ convertHardenedIndex index

accountKeyToPaymentKey :: Shelley 'AccountK XPrv -> Word32 -> Shelley 'PaymentK XPrv
accountKeyToPaymentKey acctK index = deriveAddressPrivateKey acctK UTxOExternal $ convertSoftIndex index

rootKeyToPolicyKey :: HasCallStack => Shelley 'RootK XPrv -> Word32 -> Shelley 'PolicyK XPrv
rootKeyToPolicyKey acctK index = derivePolicyPrivateKey acctK $ convertHardenedIndex index

accountKeyToStakeKey :: Shelley 'AccountK XPrv -> Word32 -> Shelley 'PaymentK XPrv
accountKeyToStakeKey acctK index = deriveAddressPrivateKey acctK Stake $ convertSoftIndex index

test :: IO ()
test = do
  eResult <- runExceptT $ do
    config <- parseConfig "/home/clever/iohk/cardano-world/sample-config.json"
    print config
    rootK <- mnemonicToRootKey $ fcfMnemonic config
    let acctK = rootKeytoAcctKey rootK 0x80000000

    let _addrK = accountKeyToPaymentKey acctK 0x14
    pure ()
  {-
  stakeK <- accountKeyToStakeKey acctK 0x1
  let
    x :: SigningKey StakeExtendedKey
    x = StakeExtendedSigningKey $ getKey stakeK
    foo' :: SigningKey PaymentExtendedKey
    foo' = PaymentExtendedSigningKey $ getKey addrK
    _foo :: SomeWitness
    _foo = APaymentExtendedSigningKey foo'
    a :: VerificationKey StakeExtendedKey
    a = getVerificationKey x
    b :: Hash StakeKey
    b = verificationKeyHash $ castVerificationKey a
    c :: StakeCredential
    c = StakeCredentialByKey b
    e :: Either String PoolId
    e = eitherDecode "\"pool1mjhwg36auxnd37mcfqlc647w92hdedejj5jalvckaxsw68tjjxw\""
    d :: PoolId
    d = fromRight (error "todo") e
    f :: Certificate
    f = makeStakeAddressDelegationCertificate c d
  print b
  print c
  print e
  print f
  addr <- paymentKeyToAddress foo' $ fcfNetwork config
  print $ serialiseAddress addr
  pure $ getVerificationKey foo'
  -}
  -- addr_test1vq4p02j5rf5w69kldld0t9wt6fe2efvfhcxsk4qdhwm2pnglr5yaj
  case eResult of
    Left err -> print $ renderFaucetError err
    Right msg -> print msg
