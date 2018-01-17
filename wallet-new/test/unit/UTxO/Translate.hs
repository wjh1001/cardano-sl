module UTxO.Translate (
    -- * Monadic context for the translation from the DSL to Cardano
    Translate
  , runTranslate
  , lift
  , liftPure
  , liftMaybe
    -- * Some lifted Cardano operations
  , genesisBlock0
  , genesisLeaders
  , genesisStakes
  , genesisBalances
  , genesisData
  , generatedSecrets
  ) where


import Universum hiding (lift)
import Control.Exception (throw)
import Control.Monad.Except (MonadError)
import System.IO.Error (userError)

import Pos.Core hiding (genesisData, generatedSecrets)
import Pos.Update
import Pos.Txp (Utxo, utxoToAddressCoinPairs)
import qualified Pos.Context     as Cardano
import qualified Pos.Core        as Cardano
import qualified Pos.Lrc.Genesis as Cardano

{-------------------------------------------------------------------------------
  Testing infrastructure from cardano-sl-core

  The genesis block comes from defaultTestConf, which in turn uses
  configuration.yaml. It is specified by a 'GenesisSpec'.
-------------------------------------------------------------------------------}

import Test.Pos.Util (
    withDefConfiguration
  , withDefUpdateConfiguration
  )

{-------------------------------------------------------------------------------
  Translation context
-------------------------------------------------------------------------------}

data Translate a = Translate {
      unTranslate :: (HasConfiguration, HasUpdateConfiguration) => Either Text a
    }

instance Functor Translate where
  fmap = liftM

instance Applicative Translate where
  pure  = return
  (<*>) = ap

instance Monad Translate where
  return a = Translate $ Right a
  x >>= f  = Translate $ case unTranslate x of
                           Left err -> Left err
                           Right a  -> unTranslate (f a)

lift :: (forall m. (HasConfiguration, HasUpdateConfiguration, MonadError Text m) => m a)
     -> Translate a
lift act = Translate act

liftPure :: ((HasConfiguration, HasUpdateConfiguration) => a) -> Translate a
liftPure a = Translate (Right a)

liftMaybe :: Text -> ((HasConfiguration, HasUpdateConfiguration) => Maybe a) -> Translate a
liftMaybe err ma = Translate $ case ma of
                                 Just a  -> Right a
                                 Nothing -> Left err

runTranslate :: Translate a -> a
runTranslate (Translate act) =
   withDefConfiguration $
   withDefUpdateConfiguration $
     case act of
       Left  e -> throw (userError (show e))
       Right a -> a

{-------------------------------------------------------------------------------
  Some cardano operations lifted to 'Translate'
-------------------------------------------------------------------------------}

genesisBlock0  :: Translate GenesisBlock
genesisData    :: Translate GenesisData
genesisLeaders :: Translate SlotLeaders
genesisStakes  :: Translate StakesMap
genesisUtxo    :: Translate Utxo

genesisBlock0  = liftPure Cardano.genesisBlock0
genesisData    = liftPure Cardano.genesisData
genesisLeaders = liftPure Cardano.genesisLeaders
genesisStakes  = liftPure Cardano.genesisStakes
genesisUtxo    = liftPure (Cardano.unGenesisUtxo Cardano.genesisUtxo)

genesisBalances :: Translate [(Address, Coin)]
genesisBalances = utxoToAddressCoinPairs <$> genesisUtxo

generatedSecrets :: Translate GeneratedSecrets
generatedSecrets = liftMaybe err Cardano.generatedSecrets
  where
    err :: Text
    err = "Generated secrets unavailable"
