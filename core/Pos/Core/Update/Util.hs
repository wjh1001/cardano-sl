-- | Utility functions related to core update system types.

module Pos.Core.Update.Util
       (
         -- * Checkers/validators.
         checkUpdatePayload
       , checkUpdateProposal
       , checkUpdateVote

       , mkUpdateProposalWSign
       , mkVoteId
       , mkUpdateProof

       -- * Formatters
       , softforkRuleF
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import           Formatting (Format, build)
import           Instances.TH.Lift ()

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Crypto ()
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Update.Types (BlockVersion, BlockVersionModifier (..), SoftforkRule,
                                        SoftwareVersion, SystemTag, UpAttributes, UpdateData,
                                        UpdatePayload (..), UpdateProof, UpdateProposal (..),
                                        UpdateProposalToSign (..), UpdateVote (..), VoteId)
import           Pos.Crypto (PublicKey, SafeSigner, SignTag (SignUSProposal, SignUSVote),
                             Signature, checkSig, hash, safeSign, safeToPublic)

checkUpdatePayload
    :: (HasConfiguration, MonadError Text m, Bi UpdateProposalToSign)
    => UpdatePayload
    -> m UpdatePayload
checkUpdatePayload it = do
    _ <- forM (upProposal it) checkUpdateProposal
    _ <- forM (upVotes it) checkUpdateVote
    pure it

-- | 'SoftforkRule' formatter which restricts type.
softforkRuleF :: Format r (SoftforkRule -> r)
softforkRuleF = build

checkUpdateVote
    :: (HasConfiguration, MonadError Text m)
    => UpdateVote
    -> m UpdateVote
checkUpdateVote it =
    it <$ unless sigValid (throwError "UpdateVote: invalid signature")
  where
    sigValid = checkSig SignUSVote (uvKey it) (uvProposalId it, uvDecision it) (uvSignature it)

checkUpdateProposal
    :: (HasConfiguration, MonadError Text m, Bi UpdateProposalToSign)
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
        (throwError "UpdateProposal: invalid signature")

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
