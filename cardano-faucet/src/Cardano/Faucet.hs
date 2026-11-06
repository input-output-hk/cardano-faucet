{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE NumericUnderscores #-}

{- HLINT ignore "Use let" -}

module Cardano.Faucet (main) where

import Cardano.Address.Derivation (Depth (AccountK), XPrv)
import Cardano.Address.Style.Shelley (Shelley, getKey)
import Cardano.Api (
  AddressAny,
  BlockInMode,
  ChainPoint,
  LocalNodeConnectInfo,
  QueryInMode,
  QueryUTxOFilter,
  ShelleyBasedEra,
  SigningKey,
  TxId,
  TxInMode,
  TxValidationErrorInCardanoMode,
  UTxO,
  InAnyShelleyBasedEra, 
 ) 
import Cardano.Api qualified as Api
import Cardano.Api.Byron ()
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Network.IPC (LocalTxMonitorClient (..))
import Cardano.Api.Network (NetworkId)
import Cardano.Api.Certificate (PoolId)
import Cardano.Api.Key (StakeExtendedKey)
import Cardano.Api.Key qualified as Key
import Cardano.Api.Block (SlotNo)
import Cardano.Api.Address (StakeAddress, StakeCredential (..), makeStakeAddress)
import Cardano.CLI.EraIndependent.Address.Run (buildShelleyAddress)
import Cardano.Faucet.Misc
import Cardano.Faucet.Types (
  FaucetConfigFile (..),
  FaucetError (..),
  FaucetState (..),
  FaucetValue,
  StakeKeyIntermediateState (..),
  StakeKeyState (..),
  accountKeyToPaymentKey,
  accountKeyToStakeKey,
  mnemonicToRootKey,
  parseConfig,
  renderFaucetError,
  rootKeytoAcctKey,
 )
import Cardano.Faucet.Utils
import Cardano.Faucet.Web
import Cardano.Prelude hiding ((%))
import Control.Concurrent.STM (TMVar, TQueue)
import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Except.Extra (left)
import Data.List.Utils (uniq)
import Data.Map qualified as Map
import Data.Map.Merge.Lazy as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Formatting (format, (%))
import Formatting.ShortFormatters hiding (b, f, l, x)
import Network.Wai.Handler.Warp
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Net.Query
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))
import Ouroboros.Network.Protocol.LocalTxMonitor.Client qualified as CTxMon
import Ouroboros.Network.Protocol.LocalTxSubmission.Client qualified as Net.Tx
import Paths_cardano_faucet (getDataFileName)
import Protolude (print, readFile)
import Servant
import System.Environment (lookupEnv)
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import Prelude qualified
import Control.Concurrent.Async (mapConcurrently_)

-- | Starts a cardano-node client and API server. The node client keeps track of the
-- Faucet's wallet and submits transactions whenever funds are requested. The API serves a
-- simple UI and endpoints to request funds.
main :: IO ()
main = do
  -- By default, GHC uses block buffering when stdout isn't a terminal (ie, systemd).
  -- Force line buffering to keep logs up-to-date.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  fsTxQueue <- STM.newTQueueIO

  -- Look up runtime configuration
  dryRun <- (== Just "1") <$> lookupEnv "DRY_RUN"
  portStr <- fromMaybe "8090" <$> lookupEnv "PORT"
  configFilePath <- lookupEnv "CONFIG_FILE"
  -- $PORT has a default, but a specified value can still contain nonsense 
  -- (eg, "bogus-port"), so we still have to validate it.
  let port =
        fromMaybe 
          (panic $ "invalid $PORT: " <> toText portStr)
          (readMaybe portStr)

  -- To calculate the faucet state, we have to query the current node's era, so we pass
  -- this to query client. The API server waits for it to be filled before starting.
  stateVar <- STM.newEmptyTMVarIO

  result <- runExceptT $ do
    -- Parse the config file, an error is returned if $CONFIG_FILE is not set.
    configFile <- unmaybe configFilePath
    fsConfig <- parseConfig configFile

    Right sockPath <- liftIO readEnvSocketPath
    let
      localNodeConnInfo :: LocalNodeConnectInfo
      localNodeConnInfo = 
        Api.LocalNodeConnectInfo 
          defaultCModeParams 
          (fcfNetwork fsConfig)
          (Api.File sockPath)

      runNodeClient :: IO ()
      runNodeClient = 
        Api.connectToLocalNode
          localNodeConnInfo
          Api.LocalNodeClientProtocols
            { localChainSyncClient = Api.NoLocalChainSyncClient
            , localStateQueryClient = Just (queryClient fsConfig stateVar fsTxQueue)
            , localTxSubmissionClient = Just (submissionClient dryRun fsTxQueue)
            , localTxMonitoringClient = Just (txMonitor fsConfig)
            }

      runApiServer :: IO ()
      runApiServer = do
        Api.InAnyShelleyBasedEra era faucetState <- atomically $ STM.takeTMVar stateVar
        putStrLn $ format ("lovelace values for api keys " % sh) $ fsBucketSizes faucetState
        putStrLn $ "faucet address: " <> Api.serialiseAddress (fsOwnAddress faucetState)
        startApiServer era faucetState port

    -- Orchestrate node client and API server in separate threads. If either aborts, the
    -- other one exits.
    liftIO $
      mapConcurrently_ identity
        [ runNodeClient
        , runApiServer
        ]

  either (putStrLn . renderFaucetError) pure result

-- | Evaluates a configuration 'FilePath'. If not present, it returns an error.
unmaybe :: Maybe Prelude.String -> ExceptT FaucetError IO Prelude.String
unmaybe (Just path) = pure path
unmaybe Nothing = left FaucetErrorConfigFileNotSet

-- | Reads $CARDANO_NODE_SOCKET_PATH environment variable. If unset, it returns an error.
readEnvSocketPath :: IO (Either Text Prelude.String)
readEnvSocketPath = do
  mEnvName <- lookupEnv envName
  case mEnvName of
    Just sPath -> return $ Right sPath
    Nothing -> return . Left $ Text.pack envName
  where
    envName :: Prelude.String
    envName = "CARDANO_NODE_SOCKET_PATH"

startApiServer ::
  ShelleyBasedEra era ->
  FaucetState era ->
  Port ->
  IO ()
startApiServer era faucetState port = do
  let
    -- Bind both ipv4 and ipv6
    settings = setTimeout 600 $ setHost "*6" $ setPort port defaultSettings
  index_path <- getDataFileName "index.html"
  print index_path
  index_html <- readFile index_path

  runSettings settings (app era faucetState index_html)

app :: ShelleyBasedEra era -> FaucetState era -> Text -> Application
app era faucetState indexHtml = serve userAPI $ server era faucetState indexHtml

-- | Local node state query; Looks up the current era, calculates the faucet state and
-- utxo set, then periodically updates the stake keys.
queryClient ::
  FaucetConfigFile ->
  TMVar (InAnyShelleyBasedEra FaucetState) ->
  TQueue (TxInMode, ByteString, TxId) ->
  Net.Query.LocalStateQueryClient BlockInMode ChainPoint QueryInMode IO ()
queryClient config stateVar txQueue = Api.LocalStateQueryClient $ do
  aquireConnection $ do
    runQueryThen Api.QueryCurrentEra $ \(Api.AnyCardanoEra era) -> do
      case cardanoEraToShelleyBasedEra era of
        Left err -> Prelude.error $ Text.unpack err
        Right sbe -> do
          eFaucetState <- liftIO $ runExceptT $ newFaucetState config txQueue
          let
            faucetState = fromRight (Prelude.error "cant create state") eFaucetState
          atomically $ STM.putTMVar stateVar (Api.inAnyShelleyBasedEra sbe faucetState)
          runQueryThen (getUtxoQuery sbe (fsOwnAddress faucetState)) $ \case
            Right result -> do
              let
              -- reduceTxo :: TxOut ctx era -> (L.Coin, TxOut ctx era)
              -- reduceTxo out@(TxOut _ value _ _) = (getValue value, out)
              -- reducedUtxo :: Map TxIn (L.Coin, TxOut CtxUTxO era)
              -- reducedUtxo = Map.map reduceTxo $ unUTxO result
              -- atomically $ putTMVar utxoTMVar $ unUTxO result
              let stats = computeUtxoStats (Api.unUTxO result)
              print stats
              atomically $ STM.putTMVar (fsUtxoTMVar faucetState) (Api.unUTxO result)
              putStrLn @Text "utxo set initialized"

              case fcfMaxStakeKeyIndex config of
                Nothing -> finish
                Just count -> do
                  let
                    manyStakeKeys :: Map StakeAddress (Word32, SigningKey StakeExtendedKey, StakeCredential)
                    manyStakeKeys = createManyStakeKeys (fsAcctKey faucetState) (fcfNetwork config) count
                  queryStakeKeyLoop sbe (fcfNetwork config) manyStakeKeys (fcfDebug config) faucetState True
            Left e -> Prelude.error $ show e

-- | Acquires a connection and runs the specified action
aquireConnection ::
  Applicative f =>
  m (Net.Query.ClientStAcquired block point query m a) ->
  f (Net.Query.ClientStIdle block point query m a)
aquireConnection aquireComplete = do
  pure
    $ Net.Query.SendMsgAcquire VolatileTip
    $ Net.Query.ClientStAcquiring
      { Net.Query.recvMsgAcquired = aquireComplete
      , Net.Query.recvMsgFailure = Prelude.error "not implemented"
      }

-- | Reacquires a connection and runs the specified action
reAcquireThen ::
  m (Net.Query.ClientStAcquired block point query m a) ->
  IO (Net.Query.ClientStAcquired block point query m a)
reAcquireThen cb = do
  pure
    $ Net.Query.SendMsgReAcquire VolatileTip
    $ Net.Query.ClientStAcquiring
      { Net.Query.recvMsgAcquired = cb
      , Net.Query.recvMsgFailure = Prelude.error "not implemented"
      }

-- | Runs a query, passing the result to the specified continuation.
runQueryThen ::
  query t ->
  (t -> IO (Net.Query.ClientStAcquired block point query IO a)) ->
  IO (Net.Query.ClientStAcquired block point query IO a)
runQueryThen query queryDone = do
  pure
    $ Net.Query.SendMsgQuery query
    $ Net.Query.ClientStQuerying
      { Net.Query.recvMsgResult = \result -> do
          queryDone result
      }

-- | Runs idle, keeping the state query connection alive
finish :: IO (Net.Query.ClientStAcquired block point query IO ())
finish = do
  void . forever $ threadDelay 43_200 {- day in seconds -}
  -- TODO: this can never be reached!
  pure
    $ Net.Query.SendMsgRelease
    $ pure
    $ Net.Query.SendMsgDone ()

-- | Queries an address for UTxOs
getUtxoQuery :: ShelleyBasedEra era -> AddressAny -> QueryInMode (Either EraMismatch (UTxO era))
getUtxoQuery sbe address = Api.QueryInEra query
  where
    qfilter :: QueryUTxOFilter
    qfilter = Api.QueryUTxOByAddress $ Set.singleton address
    query = Api.QueryInShelleyBasedEra sbe (Api.QueryUTxO qfilter)

newFaucetState ::
  FaucetConfigFile -> TQueue (TxInMode, ByteString, TxId) -> ExceptT FaucetError IO (FaucetState era)
newFaucetState fsConfig fsTxQueue = do
  (fsUtxoTMVar, fsStakeTMVar, fsSendMoneyRateLimitState, fsDelegationRateLimitState) <-
    liftIO $ (,,,) 
      <$> STM.newEmptyTMVarIO 
      <*> STM.newEmptyTMVarIO 
      <*> STM.newTMVarIO mempty 
      <*> STM.newTMVarIO mempty
  fsRootKey <- mnemonicToRootKey $ fcfMnemonic fsConfig
  let
    fsAcctKey = rootKeytoAcctKey fsRootKey 0x80000000
    addrK = accountKeyToPaymentKey fsAcctKey (fcfAddressIndex fsConfig)
    pay_skey = Api.PaymentExtendedSigningKey $ getKey addrK
    pay_vkey = Api.getVerificationKey pay_skey
    fsPaymentSkey = Api.WitnessPaymentExtendedKey pay_skey
    fsPaymentVkey = pay_vkey
    fsBucketSizes = findAllSizes fsConfig
    fsNetwork = fcfNetwork fsConfig

  shelleyAddress <-
    withExceptT FaucetErrorTodo2 $
      runInCIO () $
          buildShelleyAddress (Key.castVerificationKey pay_vkey) Nothing fsNetwork

  let fsOwnAddress = Api.AddressShelley shelleyAddress

  pure $ FaucetState {..}

-- | Returns all API/recaptcha limit values
findAllSizes :: FaucetConfigFile -> [FaucetValue]
findAllSizes FaucetConfigFile {fcfRecaptchaLimits, fcfApiKeys} = uniq values
  where
    values :: [FaucetValue]
    values = map toFaucetValue $ Map.elems fcfApiKeys ++ Map.elems fcfRecaptchaLimits

-- | Refresh stake keys state, runs every minute.
queryStakeKeyLoop ::
  ShelleyBasedEra era ->
  NetworkId ->
  Map StakeAddress (Word32, SigningKey StakeExtendedKey, StakeCredential) ->
  Bool ->
  FaucetState era ->
  Bool ->
  IO (Net.Query.ClientStAcquired block point QueryInMode IO ())
queryStakeKeyLoop era network manyStakeKeys debug faucetState initial = do
  let
    stakeCredentials :: [StakeCredential]
    stakeCredentials = Map.elems $ map (\(_, _, v) -> v) manyStakeKeys
  runQueryThen (queryManyStakeAddr era network stakeCredentials) $ \case
    Right stakeKeyResults -> do
      let (notRegistered, notDelegated, delegated) = sortStakeKeys stakeKeyResults manyStakeKeys
      if debug
        then do
          putStrLn $ format ("these stake key indexes are not registered: " % sh) notRegistered
          putStrLn
            $ format ("these stake keys are registered and ready for use: " % sh)
            $ sort
            $ map (\(index, _skey, _vkey) -> index) notDelegated
          putStrLn $ format ("these stake keys are delegated: " % sh) $ sort delegated
        else do
          when initial
            $ putStrLn
            $ format
              ( d
                  % " stake keys not registered, "
                  % d
                  % " stake keys registered and ready for use, "
                  % d
                  % " stake keys delegated to pools"
              )
              (length notRegistered)
              (length notDelegated)
              (length delegated)
      atomically $ writeTMVar (fsStakeTMVar faucetState) (notDelegated, delegated)
      threadDelay 60_000_000
      reAcquireThen $ do
        queryStakeKeyLoop era network manyStakeKeys debug faucetState False
    Left e -> Prelude.error $ show e

-- | Query stake addresses corresponding to the specified stake credentials
queryManyStakeAddr ::
  ShelleyBasedEra era ->
  NetworkId ->
  [StakeCredential] ->
  QueryInMode (Either EraMismatch (Map StakeAddress L.Coin, Map StakeAddress PoolId))
queryManyStakeAddr sbe network creds = 
  Api.QueryInEra $
    Api.QueryInShelleyBasedEra 
      sbe 
      (Api.QueryStakeAddresses (Set.fromList creds) network)

-- | Group stake keys by `(not registered, not delegated, delegated)`
sortStakeKeys ::
  (Map StakeAddress L.Coin, Map StakeAddress PoolId) ->
  Map StakeAddress (Word32, SigningKey StakeExtendedKey, StakeCredential) ->
  ([Word32], [(Word32, SigningKey StakeExtendedKey, StakeCredential)], [(Word32, L.Coin, PoolId)])
sortStakeKeys (registeredStakeKeys, delegatedStakeKeys) manyStakeKeys = do
  let
    intermediateMerge :: Map StakeAddress StakeKeyIntermediateState
    intermediateMerge =
      Map.merge
        (mapMissing $ \_ (index, _skey, _vkey) -> StakeKeyIntermediateStateNotRegistered index)
        dropMissing
        ( zipWithMaybeAMatched $ \_key (index, skey, vkey) reward -> pure $ Just $ StakeKeyIntermediateStateRegistered (index, skey, vkey, reward)
        )
        manyStakeKeys
        registeredStakeKeys

    finalMerge :: Map StakeAddress StakeKeyState
    finalMerge =
      Map.merge
        (mapMissing onlyRegistered)
        dropMissing
        (zipWithMaybeAMatched registeredAndDelegated)
        intermediateMerge
        delegatedStakeKeys

    finalMergeValues = Map.elems finalMerge

    notRegistered :: [Word32]
    notRegistered = sort $ mapMaybe filterOnlyNotRegistered finalMergeValues

    notDelegated :: [(Word32, SigningKey StakeExtendedKey, StakeCredential)]
    notDelegated = mapMaybe filterOnlyRegistered finalMergeValues

    delegated :: [(Word32, L.Coin, PoolId)]
    delegated = mapMaybe filterOnlyDelegated finalMergeValues
  (notRegistered, notDelegated, delegated)
  where
    -- this key is not delegated
    onlyRegistered :: StakeAddress -> StakeKeyIntermediateState -> StakeKeyState
    -- and is registered, then we can use it
    onlyRegistered _key (StakeKeyIntermediateStateRegistered (index, skey, vkey, reward)) = StakeKeyRegistered index skey vkey reward
    -- but isnt registered
    onlyRegistered _key (StakeKeyIntermediateStateNotRegistered index) = StakeKeyNotRegistered index
    -- this key is delegated
    registeredAndDelegated ::
      StakeAddress -> StakeKeyIntermediateState -> PoolId -> Identity (Maybe StakeKeyState)
    -- and registered
    registeredAndDelegated _key (StakeKeyIntermediateStateRegistered (index, _skey, _vkey, rewards)) poolid = pure $ Just $ StakeKeyDelegated index rewards poolid
    -- delegated but not registered!?
    registeredAndDelegated _key (StakeKeyIntermediateStateNotRegistered _) _ = pure Nothing

    filterOnlyNotRegistered :: StakeKeyState -> Maybe Word32
    filterOnlyNotRegistered (StakeKeyNotRegistered index) = Just index
    filterOnlyNotRegistered _ = Nothing
    filterOnlyRegistered ::
      StakeKeyState -> Maybe (Word32, SigningKey StakeExtendedKey, StakeCredential)
    filterOnlyRegistered (StakeKeyRegistered index skey vkey _rewards) = Just (index, skey, vkey)
    filterOnlyRegistered _ = Nothing
    filterOnlyDelegated :: StakeKeyState -> Maybe (Word32, L.Coin, PoolId)
    filterOnlyDelegated (StakeKeyDelegated index reward poolid) = Just (index, reward, poolid)
    filterOnlyDelegated _ = Nothing

-- | Create the specified number of stake keys from an account
createManyStakeKeys ::
  Shelley 'AccountK XPrv ->
  NetworkId ->
  Word32 ->
  Map StakeAddress (Word32, SigningKey StakeExtendedKey, StakeCredential)
createManyStakeKeys acctK net count = Map.fromList $ map f indexRange
  where
    indexRange = [0 .. count]
    f :: Word32 -> (StakeAddress, (Word32, SigningKey StakeExtendedKey, StakeCredential))
    f x = (address, (x, skey, vkey))
      where
        (skey, vkey, address) = deriveSingleKey net acctK x

-- | Create a stake key from an account
deriveSingleKey ::
  NetworkId ->
  Shelley 'AccountK XPrv ->
  Word32 ->
  (SigningKey StakeExtendedKey, StakeCredential, StakeAddress)
deriveSingleKey net acctK stakeIndex = (stake_skey, y, x)
  where
    stakeK = accountKeyToStakeKey acctK stakeIndex
    stake_skey = Key.StakeExtendedSigningKey $ getKey stakeK
    stake_vkey = Api.getVerificationKey stake_skey
    stake_vkey_hash = Key.verificationKeyHash $ Key.castVerificationKey stake_vkey
    y = StakeCredentialByKey stake_vkey_hash
    x = makeStakeAddress net y

-- | Reads transactions from a queue and submits them
submissionClient ::
  Bool ->
  TQueue (TxInMode, ByteString, TxId) ->
  Net.Tx.LocalTxSubmissionClient TxInMode TxValidationErrorInCardanoMode IO a2
submissionClient dryRun txQueue = Net.Tx.LocalTxSubmissionClient waitForTxAndLoop
  where
    waitForTxAndLoop :: IO (Net.Tx.LocalTxClientStIdle TxInMode TxValidationErrorInCardanoMode IO a)
    waitForTxAndLoop = do
      (tx, prettyTx, txid) <- atomically $ STM.readTQueue txQueue
      if dryRun
        then do
          putStrLn @Text "dry-run, not sending the following tx:"
          putStrLn prettyTx
          waitForTxAndLoop
        else pure $ Net.Tx.SendMsgSubmitTx tx $ \case
          Net.Tx.SubmitSuccess -> do
            putStrLn $ format ("tx submitted successfully: txid " % sh) txid
            waitForTxAndLoop
          Net.Tx.SubmitFail reason -> do
            putStrLn
              $ format
                ("tx submission FAILED: txid " % sh % " reason: " % sh)
                txid
                reason
            waitForTxAndLoop

-- | Prints each transaction if debug is enabled
txMonitor :: FaucetConfigFile -> LocalTxMonitorClient txid TxInMode SlotNo IO a
txMonitor FaucetConfigFile {fcfDebug} = LocalTxMonitorClient $ return $ CTxMon.SendMsgAcquire getSnapshot
  where
    getSnapshot :: SlotNo -> IO (CTxMon.ClientStAcquired txid1 TxInMode SlotNo IO a1)
    getSnapshot _slot = do
      -- when fcfDebug $ do
      -- putStrLn $ format ("got mempool snapshot at slot " % sh) $ slot
      return $ CTxMon.SendMsgNextTx getNextTx
    getNextTx :: Show tx => Maybe tx -> IO (CTxMon.ClientStAcquired txid1 TxInMode SlotNo IO a1)
    getNextTx (Just tx) = do
      when fcfDebug $ do
        putStrLn $ format ("found tx in snapshot: " % sh) tx
      return $ CTxMon.SendMsgNextTx getNextTx
    getNextTx Nothing = do
      return $ CTxMon.SendMsgAwaitAcquire getSnapshot

-- remove once stm is upgraded
-- TODO[sgillespie]: this looks like it can be removed now
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar tmvar new = STM.tryTakeTMVar tmvar >> STM.putTMVar tmvar new

