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
    it "can construct and verify empty block" $ do
      runTranslate (int emptyBlock >>= verifyFromGenesis')
        `shouldSatisfy` isRight

    it "can construct and verify block with one transaction" $
      runTranslate (int oneTrans >>= verifyFromGenesis')
        `shouldSatisfy` isRight
  where
    emptyBlock :: Chain Addr
    emptyBlock = Chain [[]]

    -- TODO: We need access to the "bootstrap transaction".
    -- TODO: We also need to make the rest available!
    t1 :: Transaction Addr
    t1 = Transaction {
             trIns  = []
           , trOuts = [Output (AddrOrdinary (IxRich 2, 0)) 10]
           }

    oneTrans :: Chain Addr
    oneTrans = Chain [[t1]]
