-- | Interpreter from the DSL to Cardano types
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module UTxO.Interpreter (
    -- * Translate the DSL to Cardano types
    Interpret(..)
    -- * Convenience re-exports
  , SlotId(..)
  ) where

import Universum hiding (lift)
import Data.Default (def)
import qualified Data.List.NonEmpty  as NE

import Pos.Block.Logic
import Pos.Client.Txp
import Pos.Core hiding (genesisData, generatedSecrets)
import Pos.Crypto
import Pos.Ssc
import Pos.Util.Chrono

import UTxO.Translate
import qualified UTxO.DSL as DSL

{-------------------------------------------------------------------------------
  Translate the DSL UTxO definitions to Cardano types

  NOTE: Delegation in Cardano is described in the document
  "Delegation and Stake Locking in Cardano SL"
  <cardano-sl-articles/delegation/pdf/article.pdf>.
-------------------------------------------------------------------------------}

class Interpret a where
  type Interpreted a :: *

  int :: a -> Translate (Interpreted a)

instance Interpret Addr where
  type Interpreted Addr = Address

  int :: Addr -> Translate Address
  int addr = actorAddr addr <$> generatedActors

instance Interpret (DSL.Input Addr) where
  type Interpreted (DSL.Input Addr) = (SecretKey, TxIn)

  int :: DSL.Input Addr -> Translate (SecretKey, TxIn)
  int inp@DSL.Input{..} =
      case DSL.outAddr (DSL.out inp) of
        DSL.AddrOrdinary owner -> do
          inpOwner' <- actorKey owner <$> generatedActors
          inpTrans' <- (hash . taTx) <$> int inpTrans
          return (
              inpOwner'
            , TxInUtxo {
                  txInHash  = inpTrans'
                , txInIndex = inpIndex
                }
            )
        _otherwise -> error "int: non-ordinary addresses not implemented"

instance Interpret (DSL.Output Addr) where
  type Interpreted (DSL.Output Addr) = TxOutAux

  int :: DSL.Output Addr -> Translate TxOutAux
  int DSL.Output{..} =
      case outAddr of
        DSL.AddrOrdinary addr -> do
          addr' <- int addr
          return TxOutAux {
              toaOut = TxOut {
                  txOutAddress = addr'
                , txOutValue   = mkCoin outVal
                }
            }
        _otherwise -> error "int: non-ordinary addresses not implemented"

instance Interpret (DSL.Transaction Addr) where
  type Interpreted (DSL.Transaction Addr) = TxAux

  -- TODO: Can we avoid FakeSigner here?
  int :: DSL.Transaction Addr -> Translate TxAux
  int DSL.Transaction{..} = do
      trIns'  <- mapM int trIns
      trOuts' <- mapM int trOuts
      liftPure $ makeMPubKeyTx
                   FakeSigner
                   (NE.fromList trIns')
                   (NE.fromList trOuts')

-- | Interpretation of a block
--
-- NOTE:
--
-- * We don't insert any delegation info in the block (not sure if we should?)
-- * We don't test the shared seed computation
-- * We stay within a single epoch for now
-- * We use the genesis block from the test configuration
--   (which has implications for which slot leaders etc we have)
instance Interpret (DSL.Block SlotId Addr) where
  type Interpreted (DSL.Block SlotId Addr) = MainBlock

  int :: DSL.Block SlotId Addr -> Translate MainBlock
  int DSL.Block{..} = do
      blockTrans' <- mapM int blockTrans

      -- empty delegation payload
      dlgPayload <- lift $ mkDlgPayload []

      -- empty update payload
      let updPayload = def

      -- previous block header
      -- if none specified, use genesis block
      prev <-
        case blockPrev of
          Just block -> (Right . view gbHeader) <$> int block
          Nothing    -> (Left  . view gbHeader) <$> genesisBlock0

      -- get block key from the slot leader
      blockKey <- getBlockKey blockSId

      lift $ createMainBlockPure
        blockSizeLimit
        prev
        Nothing -- Delegation info
        blockSId
        blockKey
        (RawPayload
            blockTrans'
            (defaultSscPayload (siSlot blockSId))
            dlgPayload
            updPayload
          )
    where
      blockSizeLimit = 1 * 1024 * 1024 -- 1 MB

instance Interpret (DSL.Chain Addr) where
  type Interpreted (DSL.Chain Addr) = OldestFirst NE Block

  int :: DSL.Chain Addr -> Translate (OldestFirst NE Block)
  int DSL.Chain{..} = do
      blocks <- (OldestFirst . NE.fromList) <$> mkBlocks Nothing 0 chainBlocks
      mapM (liftM Right . int) blocks
    where
      -- TODO: Here (and elsewhere) we assume we stay within the first epoch
      mkBlocks :: Maybe (DSL.Block SlotId Addr)
               -> Word16
               -> [[DSL.Transaction Addr]]
               -> Translate [DSL.Block SlotId Addr]
      mkBlocks _    _    []       = return []
      mkBlocks prev slot (ts:tss) = do
          lsi <- lift $ mkLocalSlotIndex slot
          let block = DSL.Block {
                          blockPrev  = prev
                        , blockTrans = ts
                        , blockSId   = SlotId {
                              siEpoch = EpochIndex 0
                            , siSlot  = lsi
                            }
                        }
          (block :) <$> mkBlocks (Just block) (slot + 1) tss
