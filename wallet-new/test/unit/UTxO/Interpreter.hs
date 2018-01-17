-- | Interpreter from the DSL to Cardano types
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module UTxO.Interpreter (
    -- * Translate the DSL to Cardano types
    Interpret(..)
    -- * Derived data
  , generatedActors
  , Actors(..)
  , Rich(..)
  , Poor(..)
  ) where

import Universum hiding (lift)
import Data.Default (def)
import Data.List ((!!))
import Formatting (bprint, build, (%))
import Serokell.Util (listJson, pairF)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map.Strict     as Map
import qualified Data.Text.Buildable

import Pos.Block.Logic
import Pos.Client.Txp
import Pos.Core hiding (genesisData, generatedSecrets)
import Pos.Crypto
import Pos.Ssc

import UTxO.Translate
import qualified UTxO.DSL as DSL

{-------------------------------------------------------------------------------
  Additional types
-------------------------------------------------------------------------------}

data Addr =
      -- | Rich actors only have a single (non-HD) address
      AddrRich Int

      -- | Poor actors however have a number of addresses (in principle, in
      -- practice currently only one)
    | AddrPoor Int Int

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

{-------------------------------------------------------------------------------
  Genesis data

  When heavy-weight delegation is enabled, 'generateGenesisData' creates three
  sets of actors, each with their own set of secret keys:

  * The 'poor' actors, with a small balance ('gsPoorSecrets')
    (these use HD addresses)
  * The 'rich' actors, with a large balance ('gsRichSecrets')
    (these do not use HD addresses)
  * The stakeholders ('gsDlgIssuersSecrets')
    (no addresses get generated for these)

  (Using the Ouroboros-neutral word "actor" intentionally to avoid confusion.)

  All addresses (for the poor and rich actors) use 'BootstrapEraDistr' as their
  stake distribution attribute; in 'bootstrapEraDistr' this is interpreted as
  a distribution over the 'gdBootStakeholders' in the genesis data, which in
  turn is derived from 'gsDlgIssuersSecrets' in 'generateGenesisData'.

  Additionally, 'generateGenesisData' generates a set of 'ProxySKHeavy' (aka
  'ProxySecretKey EpochIndex') delegating from the stakeholders (the
  'pskIssuerPk') to the rich actors (the 'pskDelegatePk'). The following excerpt
  from Section 8.2, Delegation Schema, of the Ouroboros paper is relevant here:

  > A stakeholder can transfer the right to generate blocks by creating a proxy
  > signing key that allows the delegate to sign messages of the form (st, d,
  > slj) (i.e., the format of messages signed in Protocol πDPoS to authenticate
  > a block).

  So it's actually the rich actors that sign blocks on behalf of the
  stakeholders.

  The genesis UTxO computed by 'genesisUtxo', being a Utxo, is simply a set of
  unspent transaction outputs; 'genesisStakes' then uses 'utxoToStakes' to turn
  this into a 'StakeMap'. A key component of this transaction is 'txOutStake',
  which relies on 'bootstrapEtaDistr' for addresses marked 'BootstrapEraDistr'.
  Thus the 'StakesMap' computed by 'genesisStakes' will contain 'StakeholderId's
  of the stakeholders, even though (somewhat confusingly) the stakeholders are
  never actually assigned any addresses.

  Finally, 'genesisLeaders' uses 'followTheSatoshiUtxo' applied to the
  'genesisUtxo' to compute the 'SlotLeaders' (aka 'NonEmpty StakeholderId').
  Since the stake holders have delegated their signing privilege to the rich
  actors, however, it is actually the rich actors that sign the blocks. The
  mapping from the (public keys) of the stakeholders to the (public keys) of the
  rich actors is recorded in 'gdHeavyDelegation' of 'GenesisData'.

  Concretely, the generated genesis data looks something like this:

  > { actors: Actors{
  >       rich: [
  >           Rich{ key: sec:R1S, addr: R1A}
  >         , Rich{ key: sec:R2S, addr: R2A}
  >         , Rich{ key: sec:R3S, addr: R3A}
  >         , Rich{ key: sec:R4S, addr: R4A}
  >         ]
  >     , poor: [
  >           Poor{ key: sec:P1S, addrs: [(sec:P11S, P11A)]}
  >         , Poor{ key: sec:P2S, addrs: [(sec:P21S, P21A)]}
  >         , Poor{ key: sec:P3S, addrs: [(sec:P31S, P31A)]}
  >         , Poor{ key: sec:P4S, addrs: [(sec:P41S, P41A)]}
  >         , Poor{ key: sec:P5S, addrs: [(sec:P51S, P51A)]}
  >         , Poor{ key: sec:P6S, addrs: [(sec:P61S, P61A)]}
  >         , Poor{ key: sec:P7S, addrs: [(sec:P71S, P71A)]}
  >         , Poor{ key: sec:P8S, addrs: [(sec:P81S, P81A)]}
  >         , Poor{ key: sec:P9S, addrs: [(sec:P91S, P91A)]}
  >         , Poor{ key: sec:P10S, addrs: [(sec:P101S, P101A)]}
  >         , Poor{ key: sec:P11S, addrs: [(sec:P111S, P111A)]}
  >         , Poor{ key: sec:P12S, addrs: [(sec:P121S, P121A)]}
  >         ]
  >     , stake: [
  >           Stakeholder{ id: S1Id, key: sec:S1S, del: Rich{ key: sec:R4S, addr: R4A}}
  >         , Stakeholder{ id: S2Id, key: sec:S2S, del: Rich{ key: sec:R2S, addr: R2A}}
  >         , Stakeholder{ id: S3Id, key: sec:S3S, del: Rich{ key: sec:R1S, addr: R1A}}
  >         , Stakeholder{ id: S4Id, key: sec:S4S, del: Rich{ key: sec:R3S, addr: R3A}}
  >         ]
  >     }
  > , leaders: [S1Id, S2Id, S3Id, S3Id, S1Id, S4Id, ...]
  > , stakes: [
  >       (S1Id, 11249999999999992 coin(s))
  >     , (S3Id, 11250000000000016 coin(s))
  >     , (S4Id, 11249999999999992 coin(s))
  >     , (S2Id, 11249999999999992 coin(s))
  >   ]
  > , balances: [
  >       (R1A,   11137499999752500 coin(s))
  >     , (R2A,   11137499999752500 coin(s))
  >     , (R3A,   11137499999752500 coin(s))
  >     , (R4A,   11137499999752500 coin(s))
  >     , (P11A,     37499999999166 coin(s))
  >     , (P21A,     37499999999166 coin(s))
  >     , (P31A,     37499999999166 coin(s))
  >     , (P41A,     37499999999166 coin(s))
  >     , (P51A,     37499999999166 coin(s))
  >     , (P61A,     37499999999166 coin(s))
  >     , (P71A,     37499999999166 coin(s))
  >     , (P81A,     37499999999166 coin(s))
  >     , (P91A,     37499999999166 coin(s))
  >     , (P101A,    37499999999166 coin(s))
  >     , (P111A,    37499999999166 coin(s))
  >     , (P121A,    37499999999166 coin(s))
  >     ]
  > }

  (where 'balances' also contains some "fake AVVM" balances, omitted.)
-------------------------------------------------------------------------------}

getBlockKey :: SlotId -> Translate SecretKey
getBlockKey slotId = do
    leader     <- (NE.!! slotIx) <$> genesisLeaders
    Actors{..} <- generatedActors
    return $ richKey $ stkDel (actorsStake Map.! leader)
  where
    slotIx :: Int
    slotIx = fromIntegral $ getSlotIndex (siSlot slotId)

-- | Compute generated actors
--
-- TODO: Right now this will cause a re-computation of the known actors every
-- single time. We need to fix that (and possibly allow for the actors to
-- be dynamic).
generatedActors :: Translate Actors
generatedActors = do
     genData <- genesisData
     secrets <- generatedSecrets

     let actorsRich :: Map PublicKey Rich
         actorsRich = Map.fromList
                    $ map mkRich
                    $ gsRichSecrets secrets

         actorsPoor :: Map PublicKey Poor
         actorsPoor = Map.fromList
                    $ map mkPoor
                    $ gsPoorSecrets secrets

         actorsStake :: Map StakeholderId Stakeholder
         actorsStake = Map.fromList
                     $ map (mkStake (gdHeavyDelegation genData) actorsRich)
                     $ gsDlgIssuersSecrets secrets

     return Actors{..}
  where
    -- TODO: This mapping from the secret keys to the corresponding addresses
    -- is already present in generateGenesisData , but it is not returned.
    -- I see no choice currently but to recompute it. This is unfortunate
    -- because it means that when 'generateGenesisData' changes, we'll be
    -- out of sync here. Also, we're assuming here that 'tboUseHDAddresses'
    -- is true ('useHDAddresses' must be set to true in the config yaml file).

    mkRich :: RichSecrets -> (PublicKey, Rich)
    mkRich RichSecrets{..} =
        (toPublic rsPrimaryKey, Rich {..})
      where
        richKey  = rsPrimaryKey
        richAddr = makePubKeyAddressBoot (toPublic rsPrimaryKey)

    mkPoor :: EncryptedSecretKey -> (PublicKey, Poor)
    mkPoor poorKey =
        (encToPublic poorKey, Poor {..})
      where
        poorAddrs :: [(EncryptedSecretKey, Address)]
        poorAddrs = [ case deriveFirstHDAddress
                             (IsBootstrapEraAddr True)
                             emptyPassphrase
                             poorKey of
                        Nothing          -> error "impossible"
                        Just (addr, key) -> (key, addr)
                    ]

    mkStake :: GenesisDelegation
            -> Map PublicKey Rich
            -> SecretKey
            -> (StakeholderId, Stakeholder)
    mkStake del actorsRich stkKey =
        (stkId, Stakeholder{..})
      where
        stkId :: StakeholderId
        stkId = addressHash (toPublic stkKey)

        stkDel :: Rich
        stkDel = Map.findWithDefault
                  (error ("generatedActors: delegate not found"))
                  (pskDelegatePk psk)
                  actorsRich

        psk :: ProxySKHeavy
        psk = HM.lookupDefault
                (error ("generatedActors: issuer not found"))
                stkId
                (unGenesisDelegation del)

-- | A rich actor has a key and a "simple" (non-HD) address
data Rich = Rich {
      richKey  :: SecretKey
    , richAddr :: Address
    }
  deriving (Show)

-- | A poor actor gets a HD wallet, so it has a SecretKey per address
-- (current generation just creates a single address though)
--
-- NOTE: `encToSecret :: EncryptedSecretKey -> SecretKey`
data Poor = Poor {
      poorKey   :: EncryptedSecretKey
    , poorAddrs :: [(EncryptedSecretKey, Address)]
    }
  deriving (Show)

data Stakeholder = Stakeholder {
      stkId  :: StakeholderId
    , stkKey :: SecretKey
    , stkDel :: Rich
    }
  deriving (Show)

data Actors = Actors {
      actorsRich  :: Map PublicKey Rich
    , actorsPoor  :: Map PublicKey Poor
    , actorsStake :: Map StakeholderId Stakeholder
    }
  deriving (Show)

actorAddr :: Addr -> Actors -> Address
actorAddr (AddrRich i)   Actors{..} = richAddr (Map.elems actorsRich !! i)
actorAddr (AddrPoor i j) Actors{..} = snd (poorAddrs (Map.elems actorsPoor !! i) !! j)

actorKey :: Addr -> Actors -> SecretKey
actorKey (AddrRich i)   Actors{..} = richKey (Map.elems actorsRich !! i)
actorKey (AddrPoor i j) Actors{..} = encToSecret (fst (poorAddrs (Map.elems actorsPoor !! i) !! j))

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable Rich where
  build Rich{..} = bprint
      ( "Rich"
      % "{ key: "  % build
      % ", addr: " % build
      % "}"
      )
      richKey
      richAddr

instance Buildable Poor where
  build Poor{..} = bprint
      ( "Poor"
      % "{ key: "   % build
      % ", addrs: " % listJson
      % "}"
      )
      (encToSecret poorKey)
      (map (bprint pairF . first encToSecret) poorAddrs)

instance Buildable Stakeholder where
  build Stakeholder{..} = bprint
      ( "Stakeholder"
      % "{ id: "  % build
      % ", key: " % build
      % ", del: " % build
      % "}"
      )
      stkId
      stkKey
      stkDel

instance Buildable Actors where
  build Actors{..} = bprint
      ( "Actors"
      % "{ rich: "  % listJson
      % ", poor: "  % listJson
      % ", stake: " % listJson
      % "}"
      )
      (Map.elems actorsRich)
      (Map.elems actorsPoor)
      (Map.elems actorsStake)
