module Main where

import Universum
import Formatting (format, build)
import Test.Hspec

import UTxO.DSL
import UTxO.Translate
import UTxO.Interpreter

main :: IO ()
main = do
    putStrLn $ runTranslate (getContext >>= \tc -> liftPure $ format build tc)
    hspec tests

tests :: Spec
tests = describe "Wallet unit tests" $ do
    testSanityChecks

testSanityChecks :: Spec
testSanityChecks = describe "Test sanity checks" $ do
    it "can verify constructed empty block" $ do
      runTranslate (int emptyBlock >>= verifyFromGenesis')
        `shouldSatisfy` isRight
  where
    emptyBlock :: Chain Addr
    emptyBlock = Chain [[]]
