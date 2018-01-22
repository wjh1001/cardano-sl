-- | Delegation helpers.

module Pos.Core.Delegation.Util
       ( checkDlgPayload
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.List (groupBy)
import           Formatting (sformat, (%))
import           Serokell.Util (listJson)

import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Delegation.Types (DlgPayload (..), ProxySKHeavy)
import           Pos.Crypto (ProxySecretKey (..), verifyPsk)

-- | Verifier of 'DlgPayload' which ensures absence of duplicates, or invalid
-- PSKs.
checkDlgPayload :: (HasConfiguration, MonadError Text m) => DlgPayload -> m DlgPayload
checkDlgPayload it = do
    unless (null duplicates) $
        throwError "Some of block's PSKs have the same issuer, which is prohibited"
    unless (null wrongPSKs) $ throwError $
        sformat ("At least some PSKs in the block are corrupted/broken: "%listJson)
                (take 5 wrongPSKs)
    pure it
  where
    proxySKs = getDlgPayload it
    proxySKsDups psks =
        filter (\x -> length x > 1) $
        groupBy ((==) `on` pskIssuerPk) $ sortOn pskIssuerPk psks
    duplicates = proxySKsDups proxySKs
    wrongPSKs = filter (not . verifyPsk) proxySKs
