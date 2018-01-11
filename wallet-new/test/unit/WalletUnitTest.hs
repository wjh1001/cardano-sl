module Main where

import Universum
import Formatting
import Serokell.Util (listJson, pairF)
import qualified Data.HashMap.Strict as HM

import UTxO.Interpreter
import UTxO.Translate

main :: IO ()
main = putStrLn $ runTranslate $ do
    actors   <- generatedActors
    leaders  <- genesisLeaders
    stakes   <- genesisStakes
    balances <- genesisBalances
    liftPure $ format ( "{ actors: "  % build
                      % ", leaders: " % listJson
                      % ", stakes: " % listJson
                      % ", balances: " % listJson
                      % "}"
                      )
                      actors
                      leaders
                      (map (bprint pairF) (HM.toList stakes))
                      (map (bprint pairF) balances)
