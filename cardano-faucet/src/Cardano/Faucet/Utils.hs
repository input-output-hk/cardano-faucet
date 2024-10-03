{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Faucet.Utils where

import Cardano.Api (
  AnyCardanoEra (..),
  CardanoEra (..),
  CtxUTxO,
  ShelleyBasedEra,
  Tx,
  TxFee (..),
  TxIn,
  TxOut (TxOut),
  TxValidityLowerBound (TxValidityNoLowerBound),
  TxValidityUpperBound,
  defaultTxValidityUpperBound,
  shelleyBasedEraConstraints,
  toCardanoEra,
 )
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley (ShelleyBasedEra (..))
import Cardano.CLI.Json.Friendly qualified as CLI
import Cardano.CLI.Types.MonadWarning qualified as CLI
import Cardano.Faucet.Misc
import Cardano.Faucet.Types
import Cardano.Prelude hiding ((%))
import Control.Concurrent.STM (TMVar, putTMVar, takeTMVar)
import Control.Monad.Trans.Except.Extra (left)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Prelude qualified

computeUtxoStats :: Map TxIn (TxOut CtxUTxO era) -> UtxoStats
computeUtxoStats utxo = do
  let
    convertValue :: TxOut ctx era -> FaucetValue
    -- TODO, also report tokens in this list/type
    convertValue (TxOut _ value _ _) = getValue value
  -- folder :: UtxoStats -> FaucetValue -> UtxoStats
  -- folder (UtxoStats m) v = UtxoStats $ Map.insert v ((fromMaybe 0 $ Map.lookup v m) + 1) m
  UtxoStats $ Map.fromList $ countLength $ sort $ map convertValue $ Map.elems utxo

-- foldl' folder (UtxoStats mempty) $ concat $ map convertValue $ Map.elems utxo

countDuplicates :: Ord a => [a] -> [(a, Int)]
countDuplicates = map fn . group . sort
  where
    fn (x : xs) = (x, 1 + length xs)
    fn [] = Prelude.error "not possible"

-- from https://hackage.haskell.org/package/polysemy-plugin-0.4.3.1/docs/Polysemy-Plugin-Fundep-Utils.html#v:countLength

-- | Count the number of times 'a' is present in the list.
countLength :: Eq a => [a] -> [(a, Int)]
countLength as =
  let grouped = group as
   in zipWith (curry $ bimap Prelude.head Prelude.length) grouped grouped

takeOneUtxo :: TMVar (Map TxIn (TxOut ctx era)) -> FaucetValue -> STM (Maybe (TxIn, TxOut ctx era))
takeOneUtxo utxoTMVar value = do
  utxo <- takeTMVar utxoTMVar
  let
    unwrap :: TxOut ctx1 era1 -> FaucetValue
    unwrap (TxOut _ val _ _) = getValue val
    utxoOfRightSize = Map.filter (\out -> unwrap out == value) utxo
    mTxin = head $ Map.toList $ Map.take 1 utxoOfRightSize
  case mTxin of
    Just (txin, txout) -> do
      let
        trimmedUtxo = Map.delete txin utxo
      putTMVar utxoTMVar trimmedUtxo
      pure $ Just (txin, txout)
    Nothing -> do
      putTMVar utxoTMVar utxo
      pure Nothing

findUtxoOfSize ::
  TMVar (Map TxIn (TxOut CtxUTxO era)) -> FaucetValue -> STM (TxIn, TxOut CtxUTxO era)
findUtxoOfSize utxoTMVar value = do
  -- TODO, include fee
  mTxinout {- liftIO $ atomically $ -} <- takeOneUtxo utxoTMVar value
  case mTxinout of
    Just txinout -> pure txinout
    Nothing -> throwSTM $ FaucetWebErrorUtxoNotFound value

validateTxFee ::
  ShelleyBasedEra era ->
  Maybe L.Coin ->
  ExceptT FaucetWebError IO (TxFee era)
validateTxFee sbe mfee =
  case mfee of
    Nothing -> txFeatureMismatch sbe -- Fees are explicit since Shelley
    Just fee -> return $ TxFeeExplicit sbe fee

txFeatureMismatch ::
  ShelleyBasedEra era ->
  ExceptT FaucetWebError IO a
txFeatureMismatch sbe =
  left
    $ FaucetWebErrorFeatureMismatch
    $ shelleyBasedEraConstraints sbe AnyCardanoEra
    $ toCardanoEra sbe

noBoundsIfSupported ::
  ShelleyBasedEra era ->
  (TxValidityLowerBound era, TxValidityUpperBound era)
noBoundsIfSupported sbe = (TxValidityNoLowerBound, defaultTxValidityUpperBound sbe)

prettyFriendlyTx ::
  ShelleyBasedEra era ->
  Tx era ->
  BS.ByteString
prettyFriendlyTx sbe tx =
  CLI.friendlyBS CLI.FriendlyJson prettyTxAeson
  where
    era = toCardanoEra sbe
    prettyTxAeson = fst $ runState (CLI.runWarningStateT $ CLI.friendlyTxImpl era tx) []

-- | @cardanoEraToShelleyBasedEra@ converts a 'CardanoEra' to a 'ShelleyBasedEra'
-- or returns an error message if the era is not Shelley based.
cardanoEraToShelleyBasedEra :: CardanoEra era -> Either Text (ShelleyBasedEra era)
cardanoEraToShelleyBasedEra = \case
  ByronEra -> Left "Byron is not a Shelley based era"
  ShelleyEra -> Right ShelleyBasedEraShelley
  AllegraEra -> Right ShelleyBasedEraAllegra
  MaryEra -> Right ShelleyBasedEraMary
  AlonzoEra -> Right ShelleyBasedEraAlonzo
  BabbageEra -> Right ShelleyBasedEraBabbage
  ConwayEra -> Right ShelleyBasedEraConway
