module Main
  ( main
  ) where

import           Universum

import           System.IO (hSetEncoding, stdout, utf8)

import           Bench.Pos.Wallet.GetHistoryBench (runGetHistoryBenchmark)
import           Client.Pos.Wallet.Web.Api

-- stack bench cardano-sl-explorer
main :: IO ()
main = do
    hSetEncoding stdout utf8
    putText "HI!"
    runGetHistoryBenchmark
