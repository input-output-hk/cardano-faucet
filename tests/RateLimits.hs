{-# LANGUAGE OverloadedStrings #-}

-- Run in the faucet development environment:
-- runghc -XNoImplicitPrelude -XOverloadedStrings -icardano-faucet/src tests/RateLimits.hs
module Main (main) where

import Prelude
import Cardano.Faucet.Types
import Cardano.Faucet.Web (checkRateLimits)
import Control.Concurrent.STM
import Control.Monad (unless, forM_)
import Data.Aeson (eitherDecode)
import Data.Time.Clock (addUTCTime)

main :: IO ()
main = do
  legacy <- decode "{\"api_key\":\"test\",\"lovelace\":10000200000,\"rate_limit\":86400,\"delegate\":true}"
  explicit <- decode "{\"api_key\":\"test\",\"lovelace\":10000200000,\"rate_limit\":86400,\"delegation_rate_limit\":300,\"delegate\":true}"
  nullOverride <- decode "{\"api_key\":\"test\",\"lovelace\":10000200000,\"rate_limit\":86400,\"delegation_rate_limit\":null}"
  assert "legacy delegation fallback" (akvDelegationRateLimit legacy == 86400)
  assert "null delegation fallback" (akvDelegationRateLimit nullOverride == 86400)
  assert "send period unchanged" (akvRateLimit explicit == 86400)
  assert "delegation override" (akvDelegationRateLimit explicit == 300)
  assert "delegate permission still defaults to false" (not $ akvCanDelegate nullOverride)
  forM_ [Recaptcha "default", ApiKey "test"] $ \key -> do
    sends <- newTMVarIO mempty
    delegations <- newTMVarIO mempty
    let start = read "2026-10-20 00:00:00 UTC"
        addresses = [RateLimitAddressNetwork (read "::1")]
        send seconds = atomically $ checkRateLimits (addUTCTime seconds start) addresses key sends (akvRateLimit explicit)
        delegate seconds = atomically $ checkRateLimits (addUTCTime seconds start) addresses key delegations (akvDelegationRateLimit explicit)
    send 0 >>= allowed "initial send"
    delegate 0 >>= allowed "delegation independent of send"
    delegate 299 >>= denied "delegation before five minutes" 1
    delegate 301 >>= allowed "delegation after five minutes"
    send 301 >>= denied "send still blocked after five minutes" 86099
    send 86401 >>= allowed "send after one day"
    -- Denied requests must not extend the cooldown.
  putStrLn "Rate-limit regression checks passed"
 where
  decode input = either fail pure (eitherDecode input)
  assert label ok = unless ok $ fail label
  allowed _ RateLimitResultAllow = pure ()
  allowed label _ = fail label
  denied label expected (RateLimitResultDeny remaining) = assert label (remaining == expected)
  denied label _ _ = fail label
