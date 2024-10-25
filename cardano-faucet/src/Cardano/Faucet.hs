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

{- HLINT ignore "Use let" -}

module Cardano.Faucet (main) where

import Cardano.Address.Derivation (Depth (AccountK), XPrv)
import Cardano.Address.Style.Shelley (Shelley, getKey)
import Cardano.Api (
  AddressAny (AddressShelley),
  AnyCardanoEra (AnyCardanoEra),
  BlockInMode,
  ChainPoint,
  File (File),
  LocalChainSyncClient (NoLocalChainSyncClient),
  LocalNodeClientProtocols (
    LocalNodeClientProtocols,
    localChainSyncClient,
    localStateQueryClient,
    localTxMonitoringClient,
    localTxSubmissionClient
  ),
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  LocalStateQueryClient (LocalStateQueryClient),
  QueryInEra (QueryInShelleyBasedEra),
  QueryInMode (..),
  QueryInShelleyBasedEra (QueryStakeAddresses, QueryUTxO),
  QueryUTxOFilter (QueryUTxOByAddress),
  ShelleyBasedEra,
  ShelleyWitnessSigningKey (WitnessPaymentExtendedKey),
  SigningKey (PaymentExtendedSigningKey),
  TxInMode,
  UTxO (unUTxO),
  connectToLocalNode,
  getVerificationKey,
  serialiseAddress,
 )
import Cardano.Api.Byron ()
import Cardano.Api.Ledger qualified as L

import Cardano.Api.Shelley (
  LocalTxMonitorClient (..),
  NetworkId,
  PoolId,
  SigningKey (StakeExtendedSigningKey),
  SlotNo,
  StakeAddress,
  StakeCredential (StakeCredentialByKey),
  StakeExtendedKey,
  castVerificationKey,
  makeStakeAddress,
  verificationKeyHash,
 )
import Cardano.CLI.Run.Address (buildShelleyAddress)
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
import Control.Concurrent.STM (
  TMVar,
  TQueue,
  newEmptyTMVarIO,
  newTMVarIO,
  newTQueueIO,
  putTMVar,
  readTQueue,
  tryTakeTMVar,
 )
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

-- remove once stm is upgraded
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar tmvar new = tryTakeTMVar tmvar >> putTMVar tmvar new

app ::
  ShelleyBasedEra era ->
  FaucetState era ->
  Text ->
  Application
app era faucetState indexHtml = serve userAPI $ server era faucetState indexHtml

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

findAllSizes :: FaucetConfigFile -> [FaucetValue]
findAllSizes FaucetConfigFile {fcfRecaptchaLimits, fcfApiKeys} = uniq values
  where
    values :: [FaucetValue]
    values = map toFaucetValue $ Map.elems fcfApiKeys ++ Map.elems fcfRecaptchaLimits

deriveSingleKey ::
  NetworkId ->
  Shelley 'AccountK XPrv ->
  Word32 ->
  (SigningKey StakeExtendedKey, StakeCredential, StakeAddress)
deriveSingleKey net acctK stakeIndex = (stake_skey, y, x)
  where
    stakeK = accountKeyToStakeKey acctK stakeIndex
    stake_skey = StakeExtendedSigningKey $ getKey stakeK
    stake_vkey = getVerificationKey stake_skey
    stake_vkey_hash = verificationKeyHash $ castVerificationKey stake_vkey
    y = StakeCredentialByKey stake_vkey_hash
    x = makeStakeAddress net y

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

_populateStakes :: IO ()
_populateStakes = do
  eResult <- runExceptT $ do
    configFilePath <- liftIO $ lookupEnv "CONFIG_FILE"
    bar <- unmaybe configFilePath
    config <- parseConfig bar
    rootK <- mnemonicToRootKey $ fcfMnemonic config
    let
      acctK = rootKeytoAcctKey rootK 0x80000000
      net = fcfNetwork config
    print $ map (deriveSingleKey net acctK) [0 .. 10]
    pure ()
  case eResult of
    Right _ -> pure ()
    Left err -> putStrLn $ renderFaucetError err

unmaybe :: Maybe Prelude.String -> ExceptT FaucetError IO Prelude.String
unmaybe (Just path) = pure path
unmaybe Nothing = left FaucetErrorConfigFileNotSet

getUtxoQuery :: ShelleyBasedEra era -> AddressAny -> QueryInMode (Either EraMismatch (UTxO era))
getUtxoQuery sbe address = QueryInEra query
  where
    qfilter :: QueryUTxOFilter
    qfilter = QueryUTxOByAddress $ Set.singleton address
    query = QueryInShelleyBasedEra sbe (QueryUTxO qfilter)

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

submissionClient ::
  Bool -> TQueue (TxInMode, ByteString) -> Net.Tx.LocalTxSubmissionClient TxInMode reject IO a2
submissionClient dryRun txQueue = Net.Tx.LocalTxSubmissionClient waitForTxAndLoop
  where
    waitForTxAndLoop :: IO (Net.Tx.LocalTxClientStIdle TxInMode reject IO a)
    waitForTxAndLoop = do
      (tx, prettyTx) <- atomically $ readTQueue txQueue
      if dryRun
        then do
          putStrLn @Text "dry-run, not sending the following tx:"
          putStrLn prettyTx
          waitForTxAndLoop
        else pure $ Net.Tx.SendMsgSubmitTx tx $ \_result -> do
          -- print result
          waitForTxAndLoop

queryManyStakeAddr ::
  ShelleyBasedEra era ->
  NetworkId ->
  [StakeCredential] ->
  QueryInMode (Either EraMismatch (Map StakeAddress L.Coin, Map StakeAddress PoolId))
queryManyStakeAddr sbe network creds = QueryInEra (QueryInShelleyBasedEra sbe (QueryStakeAddresses (Set.fromList creds) network))

newFaucetState ::
  FaucetConfigFile -> TQueue (TxInMode, ByteString) -> ExceptT FaucetError IO (FaucetState era)
newFaucetState fsConfig fsTxQueue = do
  (fsUtxoTMVar, fsStakeTMVar, fsSendMoneyRateLimitState, fsDelegationRateLimitState) <-
    liftIO $ (,,,) <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newTMVarIO mempty <*> newTMVarIO mempty
  fsRootKey <- mnemonicToRootKey $ fcfMnemonic fsConfig
  let
    fsAcctKey = rootKeytoAcctKey fsRootKey 0x80000000
    addrK = accountKeyToPaymentKey fsAcctKey (fcfAddressIndex fsConfig)
    pay_skey = PaymentExtendedSigningKey $ getKey addrK
    pay_vkey = getVerificationKey pay_skey
    fsPaymentSkey = WitnessPaymentExtendedKey pay_skey
    fsPaymentVkey = pay_vkey
    fsBucketSizes = findAllSizes fsConfig
    fsNetwork = fcfNetwork fsConfig
  fsOwnAddress <-
    withExceptT FaucetErrorAddr
      $ AddressShelley
      <$> buildShelleyAddress (castVerificationKey pay_vkey) Nothing fsNetwork
  pure $ FaucetState {..}

finish :: IO (Net.Query.ClientStAcquired block point query IO ())
finish = do
  void . forever $ threadDelay 43200 {- day in seconds -}
  pure
    $ Net.Query.SendMsgRelease
    $ pure
    $ Net.Query.SendMsgDone ()

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
      threadDelay 60000000
      reAcquireThen $ do
        queryStakeKeyLoop era network manyStakeKeys debug faucetState False
    Left e -> Prelude.error $ show e

queryClient ::
  FaucetConfigFile ->
  TQueue (TxInMode, ByteString) ->
  Port ->
  Net.Query.LocalStateQueryClient BlockInMode ChainPoint QueryInMode IO ()
queryClient config txQueue port = LocalStateQueryClient $ do
  aquireConnection $ do
    runQueryThen QueryCurrentEra $ \(AnyCardanoEra era) -> do
      case cardanoEraToShelleyBasedEra era of
        Left err -> Prelude.error $ Text.unpack err
        Right sbe -> do
          eFaucetState <- liftIO $ runExceptT $ newFaucetState config txQueue
          let
            faucetState = fromRight (Prelude.error "cant create state") eFaucetState
          putStrLn $ format ("lovelace values for api keys " % sh) $ fsBucketSizes faucetState
          putStrLn $ "faucet address: " <> serialiseAddress (fsOwnAddress faucetState)
          _child <- forkIO $ startApiServer sbe faucetState port
          runQueryThen (getUtxoQuery sbe (fsOwnAddress faucetState)) $ \case
            Right result -> do
              let
              -- reduceTxo :: TxOut ctx era -> (L.Coin, TxOut ctx era)
              -- reduceTxo out@(TxOut _ value _ _) = (getValue value, out)
              -- reducedUtxo :: Map TxIn (L.Coin, TxOut CtxUTxO era)
              -- reducedUtxo = Map.map reduceTxo $ unUTxO result
              -- atomically $ putTMVar utxoTMVar $ unUTxO result
              let stats = computeUtxoStats (unUTxO result)
              print stats
              atomically $ putTMVar (fsUtxoTMVar faucetState) (unUTxO result)
              putStrLn @Text "utxo set initialized"

              case fcfMaxStakeKeyIndex config of
                Nothing -> finish
                Just count -> do
                  let
                    manyStakeKeys :: Map StakeAddress (Word32, SigningKey StakeExtendedKey, StakeCredential)
                    manyStakeKeys = createManyStakeKeys (fsAcctKey faucetState) (fcfNetwork config) count
                  queryStakeKeyLoop sbe (fcfNetwork config) manyStakeKeys (fcfDebug config) faucetState True
            Left e -> Prelude.error $ show e

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

readEnvSocketPath :: IO (Either Text Prelude.String)
readEnvSocketPath = do
  mEnvName <- lookupEnv envName
  case mEnvName of
    Just sPath -> return $ Right sPath
    Nothing -> return . Left $ Text.pack envName
  where
    envName :: Prelude.String
    envName = "CARDANO_NODE_SOCKET_PATH"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  fsTxQueue <- newTQueueIO
  dryRun <- maybe False (== "1") <$> lookupEnv "DRY_RUN"
  eResult <- runExceptT $ do
    configFilePath <- liftIO $ lookupEnv "CONFIG_FILE"
    mportString <- liftIO $ lookupEnv "PORT"
    let
      portString = fromMaybe "8090" mportString
      port = Prelude.read portString
    bar <- unmaybe configFilePath
    fsConfig <- parseConfig bar
    Right sockPath <- liftIO readEnvSocketPath
    let
      localNodeConnInfo :: LocalNodeConnectInfo
      localNodeConnInfo = LocalNodeConnectInfo defaultCModeParams (fcfNetwork fsConfig) (File sockPath)

    liftIO
      $ connectToLocalNode
        localNodeConnInfo
        LocalNodeClientProtocols
          { localChainSyncClient = NoLocalChainSyncClient
          , localStateQueryClient = Just (queryClient fsConfig fsTxQueue port)
          , localTxSubmissionClient = Just (submissionClient dryRun fsTxQueue)
          , localTxMonitoringClient = Just (txMonitor fsConfig)
          }
  case eResult of
    Right msg -> print msg
    Left err -> putStrLn $ renderFaucetError err
