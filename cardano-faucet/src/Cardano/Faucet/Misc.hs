{-# LANGUAGE ImportQualifiedPost #-}

module Cardano.Faucet.Misc where

import Cardano.Api (ConsensusModeParams(CardanoModeParams), EpochSlots(EpochSlots), AddressAny, parseAddressAny, CardanoEra, AssetId(AdaAssetId), Quantity, valueToList, TxOutValue (..))
import Cardano.Api.Shelley (selectLovelace, AssetId(AssetId))
import Cardano.Faucet.Types
import Cardano.Ledger.Coin (Coin)
import Cardano.Prelude
import Control.Monad.Trans.Except.Extra (left)
import Data.Text qualified as T
import Text.Parsec
import qualified Cardano.Api as Api

getValue :: TxOutValue era -> FaucetValue
getValue (TxOutValueByron ll) = Ada ll
getValue (TxOutValueShelleyBased sbe val) = convertRemaining remaining
  where
    apiValue :: Api.Value
    apiValue = Api.fromLedgerValue sbe val
    ll :: Coin
    ll = selectLovelace apiValue
    isntAda :: (AssetId, Quantity) -> Bool
    isntAda (AdaAssetId, _) = False
    isntAda (AssetId _ _, _) = True
    remaining :: [(AssetId, Quantity)]
    remaining = filter isntAda (valueToList apiValue)
    convertRemaining :: [(AssetId, Quantity)] -> FaucetValue
    convertRemaining [t] = FaucetValueMultiAsset ll (FaucetToken t)
    convertRemaining [] = Ada ll
    convertRemaining _ = FaucetValueManyTokens ll

toFaucetValue :: ApiKeyValue -> FaucetValue
toFaucetValue (ApiKeyValue _ lovelace _ Nothing _) = Ada lovelace
toFaucetValue (ApiKeyValue _ ll _ (Just t) _) = FaucetValueMultiAsset ll t

stripMintingTokens :: FaucetValue -> FaucetValue
stripMintingTokens fv@(Ada _) = fv
stripMintingTokens fv@(FaucetValueMultiAsset _ (FaucetToken _)) = fv
stripMintingTokens (FaucetValueMultiAsset ll (FaucetMintToken _)) = Ada ll
stripMintingTokens fv@(FaucetValueManyTokens _) = fv

-- returns just the lovelace component and ignores tokens
faucetValueToCoin :: FaucetValue -> Coin
faucetValueToCoin (Ada ll) = ll
faucetValueToCoin (FaucetValueMultiAsset ll _token) = ll
faucetValueToCoin (FaucetValueManyTokens ll) = ll

parseAddress :: Text -> ExceptT FaucetWebError IO AddressAny
parseAddress addr = case parse (parseAddressAny <* eof) "" (T.unpack addr) of
  Right a -> return $ a
  Left e -> left $ FaucetWebErrorInvalidAddress addr (show e)

defaultCModeParams :: ConsensusModeParams
defaultCModeParams = CardanoModeParams (EpochSlots defaultByronEpochSlots)

-- TODO: This is correct for mainnet, but may not be correct for other networks.
defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600

convertEra :: Monad m => CardanoEra era -> ExceptT FaucetWebError m era
convertEra era =
  case (toEraInMode era CardanoMode) of
    Just eraInMode -> pure eraInMode
    Nothing -> left $ FaucetWebErrorEraConversion

