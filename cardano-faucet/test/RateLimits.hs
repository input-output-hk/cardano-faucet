{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- Run in the faucet development environment:
-- cabal test cardano-faucet:rate-limits
module Main (main) where

import Prelude
import Cardano.Faucet.Types
import Cardano.Faucet.Web (checkRateLimits)
import Control.Concurrent.STM
import Control.Monad (unless, forM_)
import Data.Aeson (fromJSON, Result (..))
import Data.Time.Clock (addUTCTime)
import Data.Aeson.QQ.Simple (aesonQQ)

main :: IO ()
main = do
  let 
    legacyJson = 
      [aesonQQ|
        {
          "api_key": "test",
          "lovelace": 10000200000,
          "rate_limits": {
            "per_address": 86400,
            "global": {
              "window": 86400,
              "limit": 10
            }
          },
          "delegate": true
        }
      |]

    explicitJson =
      [aesonQQ|
        {
          "api_key": "test",
          "lovelace": 10000200000,
          "rate_limits": {
            "per_address": 86400,
            "global": {
              "window": 86400,
              "limit": 10
            }
          },
          "delegation_rate_limits":{
            "per_address": 300,
            "global": {
              "window": 300,
              "limit": 5
            }
          },
          "delegate":true
        }
      |]

    nullOverrideJson =  
      [aesonQQ|
        {
          "api_key": "test",
          "lovelace": 10000200000,
          "rate_limits": {
            "per_address": 86400,
            "global": {
              "window": 86400,
              "limit": 10
            }
          },
          "delegation_rate_limits": null
        }
      |]

  legacy <- decode legacyJson
  explicit <- decode explicitJson
  nullOverride <- decode nullOverrideJson

  assert "legacy delegation fallback" $
    akvDelegationRateLimit legacy == 
      ApiKeyRateLimits 
        (RateLimit 86400 10)
        (RateLimit 86400 1)
  assert "null delegation fallback" $
    akvDelegationRateLimit nullOverride ==
      ApiKeyRateLimits 
        (RateLimit 86400 10)
        (RateLimit 86400 1)
  assert "send period unchanged" $
    akvRateLimits explicit ==
      ApiKeyRateLimits 
        (RateLimit 86400 10)
        (RateLimit 86400 1)
  assert "delegation override" $ 
    akvDelegationRateLimit explicit ==
      ApiKeyRateLimits 
        (RateLimit 300 5)
        (RateLimit 300 1)
  assert "delegate permission still defaults to false" (not $ akvCanDelegate nullOverride)

  forM_ [Recaptcha "default", ApiKey "test"] $ \key -> do
    sends <- newTMVarIO mempty
    delegations <- newTMVarIO mempty
    let start = read "2026-10-20 00:00:00 UTC"
        addresses = [RateLimitAddressNetwork (read "::1")]

        send seconds = 
          atomically $ 
            checkRateLimits 
              (addUTCTime seconds start)
              addresses
              key
              sends
              (akvRateLimits explicit)
        delegate seconds = 
          atomically $ 
            checkRateLimits
              (addUTCTime seconds start)
              addresses
              key
              delegations
              (akvDelegationRateLimit explicit)

    send 0 >>= allowed "initial send"
    delegate 0 >>= allowed "delegation independent of send"
    delegate 299 >>= denied "delegation before five minutes" 1
    delegate 301 >>= allowed "delegation after five minutes"
    send 301 >>= denied "send still blocked after five minutes" 86099
    send 86401 >>= allowed "send after one day"
    -- Denied requests must not extend the cooldown.

    globalState <- newTMVarIO mempty
    let globalLimits = ApiKeyRateLimits (RateLimit 60 2) (RateLimit 1 1)
        request seconds ip = atomically $
          checkRateLimits
            (addUTCTime seconds start)
            [RateLimitAddressNetwork (read ip)]
            key
            globalState
            globalLimits

    request 0 "::1" >>= allowed "first global slot"
    request 10 "::2" >>= allowed "second global slot"
    historyBeforeDenial <- atomically $ readTMVar globalState
    request 20 "::3" >>= denied "global limit spans addresses" 40
    historyAfterDenial <- atomically $ tryReadTMVar globalState
    assert "denial preserves the TMVar and history" $
      historyAfterDenial == Just historyBeforeDenial

    request 60 "::3" >>= allowed "oldest access expires at window boundary"
    request 61 "::4" >>= denied "second access still occupies a slot" 9
    request 70 "::4" >>= allowed "denial does not extend global cooldown"
    history <- atomically $ readTMVar globalState
    assert "expired global accesses are removed from stored history" $
      sum (fmap length (uhGlobal history)) == 2

  putStrLn "Rate-limit regression checks passed"
 where
  decode val = case fromJSON val of
    Success res -> pure res
    Error err -> fail err
  assert label ok = unless ok $ fail label
  allowed _ RateLimitResultAllow = pure ()
  allowed label _ = fail label
  denied label expected (RateLimitResultDeny remaining) = assert label (remaining == expected)
  denied label _ _ = fail label
