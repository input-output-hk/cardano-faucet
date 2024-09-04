module Cardano.Faucet.TxUtils where

import Cardano.Api (
  AddressAny,
  BuildTx,
  BuildTxWith (BuildTxWith),
  CtxUTxO,
  KeyWitnessInCtx (KeyWitnessForSpending),
  ShelleyBasedEra,
  ShelleyWitnessSigningKey,
  Tx,
  TxAuxScripts (TxAuxScriptsNone),
  TxBody,
  TxBodyContent (TxBodyContent),
  TxCertificates,
  TxExtraKeyWitnesses (TxExtraKeyWitnessesNone),
  TxId,
  TxIn,
  TxInsCollateral (TxInsCollateralNone),
  TxInsReference (TxInsReferenceNone),
  TxMetadataInEra (TxMetadataNone),
  TxMintValue (..),
  TxOut (TxOut),
  TxReturnCollateral (TxReturnCollateralNone),
  TxScriptValidity (TxScriptValidityNone),
  TxTotalCollateral (TxTotalCollateralNone),
  TxUpdateProposal (TxUpdateProposalNone),
  TxValidityLowerBound (TxValidityNoLowerBound),
  TxWithdrawals (TxWithdrawalsNone),
  Witness (KeyWitness),
  defaultTxValidityUpperBound,
  docToText,
  getTxId,
  makeShelleyKeyWitness,
  makeSignedTransaction,
 )
import qualified Cardano.Api.Ledger as L
import Cardano.Api.Shelley (Value, createAndValidateTransactionBody, lovelaceToValue)
import Cardano.CLI.EraBased.Run.Transaction
import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Errors.TxCmdError
import Cardano.Faucet.Misc (faucetValueToLovelace, getValue)
import Cardano.Faucet.Types (FaucetValue, FaucetWebError (..))
import Cardano.Faucet.Utils
import Cardano.Prelude hiding ((%))
import Control.Monad.Trans.Except.Extra (left)

getMintedValue :: TxMintValue BuildTx era -> Value
getMintedValue (TxMintValue _ val _) = val
getMintedValue TxMintNone = mempty

newtype Fee = Fee L.Coin

txBuild ::
  () =>
  ShelleyBasedEra era ->
  (TxIn, TxOut CtxUTxO era) ->
  Either AddressAny [TxOutAnyEra] ->
  TxCertificates BuildTx era ->
  TxMintValue BuildTx era ->
  Fee ->
  ExceptT FaucetWebError IO (TxBody era)
txBuild sbe (txin, txout) addressOrOutputs certs minting (Fee fixedFee) = do
  let
    -- localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId sockPath
    unwrap :: TxOut ctx1 era1 -> FaucetValue
    unwrap (TxOut _ val _ _) = getValue val
    value :: L.Coin
    value = faucetValueToLovelace $ unwrap txout
    change :: L.Coin
    change = value - fixedFee
    mintedValue = getMintedValue minting
    -- TODO, add minted tokens
    changeValue :: Value
    changeValue = lovelaceToValue change <> mintedValue

    getTxOuts :: Either AddressAny [TxOutAnyEra] -> [TxOutAnyEra]
    getTxOuts (Left addr) = [TxOutAnyEra addr changeValue TxOutDatumByNone ReferenceScriptAnyEraNone]
    getTxOuts (Right outs) = outs

  txBodyContent <-
    TxBodyContent
      <$> pure [(txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)]
      <*> pure TxInsCollateralNone
      <*> pure TxInsReferenceNone
      <*> mapM
        (\x -> withExceptT (FaucetWebErrorTodo . docToText . renderTxCmdError) $ toTxOutInAnyEra sbe x)
        (getTxOuts addressOrOutputs)
      <*> pure TxTotalCollateralNone
      <*> pure TxReturnCollateralNone
      <*> validateTxFee sbe (Just fixedFee)
      <*> pure TxValidityNoLowerBound
      <*> pure (defaultTxValidityUpperBound sbe)
      <*> pure TxMetadataNone
      <*> pure TxAuxScriptsNone
      <*> pure TxExtraKeyWitnessesNone
      <*> pure (BuildTxWith Nothing)
      <*> pure TxWithdrawalsNone
      <*> pure certs
      <*> pure TxUpdateProposalNone
      <*> pure minting
      <*> pure TxScriptValidityNone
      <*> pure Nothing
      <*> pure Nothing
      <*> pure Nothing
      <*> pure Nothing

  case createAndValidateTransactionBody sbe txBodyContent of
    Left err -> left $ FaucetWebErrorTodo $ show err
    Right txbody -> pure txbody

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
  () =>
  ShelleyBasedEra era ->
  TxBody era ->
  [ShelleyWitnessSigningKey] ->
  Tx era
txSign era txBody sks = tx
  where
    -- let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

    shelleyKeyWitnesses = map (makeShelleyKeyWitness era txBody) sks
    tx = makeSignedTransaction shelleyKeyWitnesses txBody

makeAndSignTx ::
  () =>
  ShelleyBasedEra era ->
  (TxIn, TxOut CtxUTxO era) ->
  Either AddressAny [TxOutAnyEra] ->
  [ShelleyWitnessSigningKey] ->
  TxCertificates BuildTx era ->
  TxMintValue BuildTx era ->
  Fee ->
  ExceptT FaucetWebError IO (Tx era, TxId)
makeAndSignTx sbe txinout addressOrOutputs skeys certs minting fee = do
  -- instead of having to specify an output that is exactly equal to input-fees
  -- i specify no outputs, and set the change addr to the end-user
  unsignedTx <- txBuild sbe txinout addressOrOutputs certs minting fee
  let
    txid :: TxId
    txid = getTxId unsignedTx
    signedTx = txSign sbe unsignedTx skeys
  pure (signedTx, txid)
