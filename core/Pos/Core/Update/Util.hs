-- | Utility functions related to core update system types.

module Pos.Core.Update.Util
       (
         -- * Checkers/validators.
         checkUpdateProposal
       , checkUpdateVote

       , mkUpdateProposalWSign
       , mkVoteId
       , mkUpdateProof

       -- * Formatters
       , softforkRuleF
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Formatting (Format, build)
import           Instances.TH.Lift ()

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Crypto ()
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Update.Types (BlockVersion, BlockVersionModifier (..), SoftforkRule,
                                        SoftwareVersion, SystemTag, UpAttributes, UpdateData,
                                        UpdatePayload, UpdateProof, UpdateProposal (..),
                                        UpdateProposalToSign (..), UpdateVote (..), VoteId)
import           Pos.Crypto (PublicKey, SafeSigner, SignTag (SignUSProposal, SignUSVote),
                             Signature, checkSig, hash, safeSign, safeToPublic)

-- | 'SoftforkRule' formatter which restricts type.
softforkRuleF :: Format r (SoftforkRule -> r)
softforkRuleF = build

checkUpdateVote
    :: (HasConfiguration, MonadFail m)
    => UpdateVote
    -> m UpdateVote
checkUpdateVote it =
    it <$ unless sigValid (fail "UpdateVote: invalid signature")
  where
    sigValid = checkSig SignUSVote (uvKey it) (uvProposalId it, uvDecision it) (uvSignature it)

checkUpdateProposal
    :: (HasConfiguration, MonadFail m, Bi UpdateProposalToSign)
    => UpdateProposal
    -> m UpdateProposal
checkUpdateProposal it = do
    let toSign = UpdateProposalToSign
            (upBlockVersion it)
            (upBlockVersionMod it)
            (upSoftwareVersion it)
            (upData it)
            (upAttributes it)
    it <$ unless (checkSig SignUSProposal (upFrom it) toSign (upSignature it))
        (fail $ "UpdateProposal: invalid signature")

mkUpdateProposalWSign
    :: (HasConfiguration, Bi UpdateProposalToSign)
    => BlockVersion
    -> BlockVersionModifier
    -> SoftwareVersion
    -> HM.HashMap SystemTag UpdateData
    -> UpAttributes
    -> SafeSigner
    -> UpdateProposal
mkUpdateProposalWSign upBlockVersion upBlockVersionMod upSoftwareVersion upData upAttributes ss =
    UnsafeUpdateProposal {..}
  where
    toSign =
        UpdateProposalToSign
            upBlockVersion
            upBlockVersionMod
            upSoftwareVersion
            upData
            upAttributes
    upFrom = safeToPublic ss
    upSignature = safeSign SignUSProposal ss toSign

mkVoteId :: UpdateVote -> VoteId
mkVoteId UpdateVote{..} = (uvProposalId, uvKey, uvDecision)

mkUpdateProof
    :: Bi UpdatePayload
    => UpdatePayload -> UpdateProof
mkUpdateProof = hash
