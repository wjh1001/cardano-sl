{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module UTxO.Verify (
    -- * Verification monad
    Verify -- opaque
  , verify
    -- * Lifted cardano operations
  , verifyToil
  ) where

import Universum
import Control.Lens ((%=))
import Control.Monad.Except
import Data.Default
import System.Wlog
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Ether

import Pos.Core
import Pos.DB.Class (MonadGState(..))
import Pos.Txp.Toil.Class
import Pos.Txp.Toil.Failure
import Pos.Txp.Toil.Trans
import Pos.Txp.Toil.Types
import Pos.Txp.Toil.Utxo
import Pos.Util
import qualified Pos.Txp.Toil as Cardano

{-
import Pos.Util.Chrono
import Serokell.Util.Verify
import qualified Pos.Block.Pure  as Cardano
-}

{-------------------------------------------------------------------------------
  Verification environment
-------------------------------------------------------------------------------}

-- | Vverification environment
--
-- The verification environment contains the data that remains unchanged
-- during verification.
--
-- Unsafe constructor since the various fields must be in agreement with
-- each other. See 'verifyEnv'.
data VerifyEnv = UnsafeVerifyEnv {
      venvInitUtxo         :: Utxo
    , venvInitStakes       :: StakesMap
    , venvInitTotal        :: Coin
    , venvBlockVersionData :: BlockVersionData
    , venvLoggerName       :: LoggerName
    }

verifyEnv' :: HasGenesisData
           => Utxo
           -> BlockVersionData
           -> LoggerName
           -> VerifyEnv
verifyEnv' utxo bvd lname = UnsafeVerifyEnv {
      venvInitUtxo         =                     utxo
    , venvInitStakes       = utxoToStakes        utxo
    , venvInitTotal        = getTotalCoinsInUtxo utxo
    , venvBlockVersionData = bvd
    , venvLoggerName       = lname
    }

verifyEnv :: HasConfiguration => Utxo -> VerifyEnv
verifyEnv utxo =
    verifyEnv'
      utxo
      genesisBlockVersionData
      "verify"

{-------------------------------------------------------------------------------
  Reader monad with access to the verification environment
-------------------------------------------------------------------------------}

newtype WithVerifyEnv a = WithVerifyEnv {
      unWithVerifyEnv :: Reader VerifyEnv a
    }
  deriving (Functor, Applicative, Monad)

withVerifyEnv :: VerifyEnv -> WithVerifyEnv a -> a
withVerifyEnv env a = runReader (unWithVerifyEnv a) env

instance HasConfiguration => MonadUtxoRead WithVerifyEnv where
  utxoGet id = WithVerifyEnv $ (Map.lookup id . venvInitUtxo) <$> ask

instance MonadStakesRead WithVerifyEnv where
  getStake id   = WithVerifyEnv $ (HM.lookup id . venvInitStakes) <$> ask
  getTotalStake = WithVerifyEnv $ venvInitTotal <$> ask

instance MonadGState WithVerifyEnv where
  gsAdoptedBVData = WithVerifyEnv $ venvBlockVersionData <$> ask

instance HasLoggerName WithVerifyEnv where
  getLoggerName        = WithVerifyEnv $ venvLoggerName <$> ask
  modifyLoggerName f x = WithVerifyEnv $ local f' (unWithVerifyEnv x)
    where
      f' :: VerifyEnv -> VerifyEnv
      f' env = env { venvLoggerName = f (venvLoggerName env) }

{-------------------------------------------------------------------------------
  Verification monad

  NOTE: Ideally we'd hide 'HasConfiguration' here in the same way that we did
  for 'Translate', but this is made impossible by the superclass constraint of
  'MonadUtxoRead'.
-------------------------------------------------------------------------------}

newtype Verify e a = Verify {
      --    StateT st (ErrorT e (Reader env)) a
      -- == st -> env -> Either e (a, st)
      unVerify :: ToilT [LogEvent] (ExceptT e WithVerifyEnv) a
    }
  deriving (Functor, Applicative, Monad)

verify :: HasConfiguration => Utxo -> Verify e a -> Either e a
verify utxo ma = fst <$> verify' def (verifyEnv utxo) ma

verify' :: GenericToilModifier [LogEvent]
        -> VerifyEnv
        -> Verify e a
        -> Either e (a, GenericToilModifier [LogEvent])
verify' st env ma = withVerifyEnv env
                  $ runExceptT
                  $ Ether.runStateT' (unVerify ma) st

deriving instance HasConfiguration => MonadUtxoRead (Verify e)
deriving instance HasConfiguration => MonadUtxo     (Verify e)
deriving instance MonadStakesRead (Verify e)
deriving instance MonadStakes     (Verify e)
deriving instance MonadTxPool     (Verify e)
deriving instance MonadGState     (Verify e)
deriving instance (MonadError e)  (Verify e)

-- Possible due to HasLoggerName instance for 'StateT'' in "Pos.Util.Orphans"
deriving instance HasLoggerName (Verify e)

instance CanLog (Verify e) where
  dispatchMessage lname sev txt = Verify $
      ether $ tmExtra %= (logEvent :)
    where
      logEvent :: LogEvent
      logEvent = LogEvent lname sev txt

{-------------------------------------------------------------------------------
  Lifted operations
-------------------------------------------------------------------------------}

verifyToil :: HasConfiguration
           => EpochIndex -> Bool -> [TxAux] -> Verify ToilVerFailure TxpUndo
verifyToil = Cardano.verifyToil

{-------------------------------------------------------------------------------
  Block verification

  There appears to be only a single "pure" block verification function
  (requiring only HasConfiguration): 'Pos.Block.Pure.verifyBlocks'.
  Unfortunately, it seems this really only verifies the block envelope (maximum
  block size, unknown attributes, that sort of thing), not the transactions
  contained within. There is also

  1. 'Pos.Block.Logic.VAR.verifyBlocksPrefix'
     Requires 'MonadBlockVerify'.

  2. 'Pos.Block.Slog.Logic.slogVerifyBlocks'
     Requires 'MonadSlogVerify'.
     Called by (1) and calls 'Pos.Block.Pure.verifyBlocks'.
     Doesn't seem to do any additional verification itself.

  3. 'Pos.Ssc.Logic.VAR.sscVerifyBlocks'
     Requires 'SscGlobalVerifyMode'.
     Called by (1).
     I think this only verifies SSC stuff (shared seed computation).

  4. 'Pos.Txp.Settings.Global.tgsVerifyBlocks'
     Requires 'TxpGlobalVerifyMode'.
     Called by (1).
     This is actually just a record selector for 'TxpGlobalSettings'; see (5).
     Documented as "Verify a chain of payloads from blocks and return txp undos
     for each payload".

  5. 'Pos.Txp.Logic.Global.verifyBlocks'
     Requires 'TxpGlobalVerifyMode'.
     It seems this is the /only/ instantiation of 'tgsVerifyBlocks' (in the core
     libraries at least); thus, also called by (1).

  6. 'Pos.Delegation.Logic.VAR.dlgVerifyBlocks'
      No constraint synonym, but requires 'MonadDBRead' and 'MonadIO'.
      According to its header comment, verifies delegation logic.

  None of these are really callable in a pure context; all of them rely on
  'MonadIO', either directly, or else indirectly through the superclass
  constraints of 'MonadDBRead'. The most important one is (5).
-------------------------------------------------------------------------------}

{-
-- | Wrapper around block verification
verifyBlocks :: OldestFirst NE Block -> Translate VerificationRes
verifyBlocks blocks = do
    -- TODO: When we move to new epochs 'genesisLeaders' is no longer sufficient
    initLeaders <- genesisLeaders

    -- Not sure why block version data is explicitly passed to 'verifyBlocks';
    -- it has a 'HasConfiguration' constraint and 'HasConfiguration' implies
    -- we have access to the genesis block version data. Perhaps to support
    -- updates. For now we can just use the info from the genesis I think
    bvd <- liftPure $ Cardano.genesisBlockVersionData

    liftPure $ Cardano.verifyBlocks
                 curSlotId
                 verifyNoUnknown
                 bvd
                 initLeaders
                 blocks
  where
    -- Don't verify whether the block belongs to the future
    curSlotId :: Maybe SlotId
    curSlotId = Nothing

    -- Verify that the header has no unknown attributes
    verifyNoUnknown :: Bool
    verifyNoUnknown = True
-}
