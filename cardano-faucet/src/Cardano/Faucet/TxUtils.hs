{-# LANGUAGE GADTs #-}

module Cardano.Faucet.TxUtils where

import Cardano.Api (
  AddressAny,
  CtxUTxO,
  PolicyAssets,
  PolicyId,
  ShelleyBasedEra,
  ShelleyLedgerEra,
  ShelleyWitnessSigningKey,
  Tx (ShelleyTx),
  TxId,
  TxIn,
  TxOut (TxOut),
  getTxBody,
  getTxId,
 )
import qualified Cardano.Api.Ledger as L
import Cardano.Api.Experimental (
  Era,
  LedgerEra,
  SimpleScriptOrReferenceInput (SScript),
  deserialiseSimpleScript,
  obtainCommonConstraints,
  sbeToEra,
 )
import Cardano.Api.Experimental.AnyScriptWitness (AnyScriptWitness (AnyScriptWitnessSimple))
import qualified Cardano.Api.Experimental.Certificate as ExpCert
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Data.Map.Strict as Map
import Cardano.Api.Value (Value, lovelaceToValue)
import Cardano.CLI.Type.Common
import Cardano.Faucet.Misc (faucetValueToLovelace, getValue)
import Cardano.Faucet.Types (FaucetValue, FaucetWebError (..))
import Cardano.Faucet.Utils
import Cardano.Prelude hiding ((%))
import Control.Monad.Trans.Except.Extra (left)
import Cardano.CLI.Compatible.Transaction.TxOut (toTxOutInAnyEra)

newtype Fee = Fee L.Coin

txBuild ::
  ShelleyBasedEra era ->
  Era era ->
  (TxIn, TxOut CtxUTxO era) ->
  Either AddressAny [TxOutAnyEra] ->
  [(ExpCert.Certificate (ShelleyLedgerEra era), Exp.AnyWitness (ShelleyLedgerEra era))] ->
  [(PolicyId, PolicyAssets, ByteString)] ->
  Fee ->
  ExceptT FaucetWebError IO (Exp.UnsignedTx (LedgerEra era))
txBuild sbe expEra (txin, txout) addressOrOutputs certList mintEntries (Fee fixedFee) = do
  let
    unwrap :: TxOut ctx1 era1 -> FaucetValue
    unwrap (TxOut _ val _ _) = getValue val
    value :: L.Coin
    value = faucetValueToLovelace $ unwrap txout
    change :: L.Coin
    change = value - fixedFee
    changeValue :: Value
    changeValue = lovelaceToValue change

    getTxOuts :: Either AddressAny [TxOutAnyEra] -> [TxOutAnyEra]
    getTxOuts (Left addr) = [TxOutAnyEra addr changeValue TxOutDatumByNone ReferenceScriptAnyEraNone]
    getTxOuts (Right outs) = outs

  -- The faucet's outputs never carry datums, so the supplemental-datum map is empty.
  expOuts <-
    mapM
      ( \x -> do
          (expOut, _supplementalDatums) <-
            withExceptT FaucetWebErrorTodo $ runInCIO () $ toTxOutInAnyEra sbe x
          pure expOut
      )
      (getTxOuts addressOrOutputs)

  -- Old-API createTransactionBody errors on Dijkstra (extractWitnessableVotes),
  -- so build via the experimental makeUnsignedTx. Inside obtainCommonConstraints
  -- the ShelleyLedgerEra/LedgerEra equality lets the cli's ShelleyLedgerEra-typed
  -- outputs and certs feed the LedgerEra-indexed body content.
  obtainCommonConstraints expEra $ do
    -- Rebuild each mint policy's witness from the old-API script CBOR: decode it
    -- as an experimental SimpleScript for the target ledger era and wrap it as a
    -- native (simple) script witness in the mint map.
    mintWits <-
      mapM
        ( \(polId, assets, scriptCbor) -> do
            simpleScript <-
              either (left . FaucetWebErrorTodo . show) pure $
                deserialiseSimpleScript scriptCbor
            pure (polId, (assets, AnyScriptWitnessSimple (SScript simpleScript)))
        )
        mintEntries
    let txBodyContent =
          Exp.defaultTxBodyContent
            { Exp.txIns = [(txin, Exp.AnyKeyWitnessPlaceholder)]
            , Exp.txOuts = expOuts
            , Exp.txFee = fixedFee
            , Exp.txCertificates = Exp.mkTxCertificates expEra certList
            , Exp.txMintValue = Exp.TxMintValue (Map.fromList mintWits)
            }
    case Exp.makeUnsignedTx expEra txBodyContent of
      Left err -> left $ FaucetWebErrorTodo $ show err
      Right unsignedTx -> pure unsignedTx

{-
 -- keep this code for now, as an example of how to use the cardano api in a monad
eInMode <- case toEraInMode era CardanoMode of
  Just result -> return result
  Nothing -> left (FaucetWebErrorConsensusModeMismatchTxBalance (show $ AnyConsensusMode CardanoMode) (AnyCardanoEra era))

let
  utxo = UTxO $ Map.fromList [ (txin, txout) ]

(pparams, eraHistory, systemStart, stakePools) <-
  newExceptT . fmap (join . first (FaucetWebErrorAcquireFailure . show)) $
    executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> runExceptT $ do
      --UTxO utxo <- firstExceptT (_ . ShelleyTxCmdTxSubmitErrorEraMismatch) . newExceptT . queryExpr
      --  $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe
      --  $ QueryUTxO (QueryUTxOByTxIn (Set.singleton txin))

      --when (null utxo || not (txin `L.elem` Map.keys utxo)) $ do
        -- txout for txin does not exist
      --  left $ ShelleyTxCmdTxInsDoNotExist [txin]

      pparams <- firstExceptT (FaucetWebErrorEraMismatch . show) . newExceptT . queryExpr
        $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters

      eraHistory <- lift . queryExpr $ QueryEraHistory CardanoModeIsMultiEra

      systemStart <- lift $ queryExpr QuerySystemStart

      stakePools <- firstExceptT (FaucetWebErrorEraMismatch . show) . ExceptT $
        queryExpr . QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryStakePools

      return (pparams, eraHistory, systemStart, stakePools)

cAddr <- pure $ case anyAddressInEra era changeAddr of
  Just addr -> addr
  Nothing -> Prelude.error "txBuild: Byron address used: "

(BalancedTxBody balancedTxBody _ _fee) <- firstExceptT (FaucetWebErrorAutoBalance . T.pack . displayError) . hoistEither $
  makeTransactionBodyAutoBalance eInMode systemStart eraHistory pparams stakePools utxo txBodyContent cAddr Nothing

return balancedTxBody
-}

txSign ::
  ShelleyBasedEra era ->
  Era era ->
  Exp.UnsignedTx (LedgerEra era) ->
  [ShelleyWitnessSigningKey] ->
  Tx era
txSign sbe expEra unsignedTx sks =
  case Exp.signTx expEra [] shelleyKeyWitnesses unsignedTx of
    Exp.SignedTx ledgerTx -> ShelleyTx sbe ledgerTx
  where
    shelleyKeyWitnesses = map (Exp.makeKeyWitness expEra unsignedTx) sks

makeAndSignTx ::
  ShelleyBasedEra era ->
  (TxIn, TxOut CtxUTxO era) ->
  Either AddressAny [TxOutAnyEra] ->
  [ShelleyWitnessSigningKey] ->
  [(ExpCert.Certificate (ShelleyLedgerEra era), Exp.AnyWitness (ShelleyLedgerEra era))] ->
  [(PolicyId, PolicyAssets, ByteString)] ->
  Fee ->
  ExceptT FaucetWebError IO (Tx era, TxId)
makeAndSignTx sbe txinout addressOrOutputs skeys certList mintEntries fee = do
  expEra <-
    either
      (const $ left $ FaucetWebErrorTodo "makeAndSignTx: era not supported (pre-Conway)")
      pure
      (sbeToEra sbe)
  -- instead of having to specify an output that is exactly equal to input-fees
  -- i specify no outputs, and set the change addr to the end-user
  unsignedTx <- txBuild sbe expEra txinout addressOrOutputs certList mintEntries fee
  let
    signedTx = txSign sbe expEra unsignedTx skeys
    txid :: TxId
    txid = getTxId (getTxBody signedTx)
  pure (signedTx, txid)
