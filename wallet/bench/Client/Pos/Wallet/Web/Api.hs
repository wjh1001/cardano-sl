{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Client.Pos.Wallet.Web.Api
    ( getHistory
    , getWallets
    , newPayment
    ) where

import           Universum

import           Pos.Client.Txp.Util        (InputSelectionPolicy)
import           Pos.Core.Types             (Coin)
import           Pos.Wallet.Web.Api         (ApiPrefix, GetHistory, GetWallets, NewPayment)
import           Pos.Wallet.Web.ClientTypes (Addr, CAccountId (..),
                                             CId (..), CPassPhrase, CTx, CWallet,
                                             ScrollLimit, ScrollOffset, Wal)
import           Servant.API                ((:<|>) (..), (:>))
import           Servant.Client             (ClientM, client)

-- | "Benchmarking API" which includes
-- endpoints we need for benchmarking.
type WalletBenchApi = ApiPrefix :> (
     GetHistory
    :<|>
     GetWallets
    :<|>
     NewPayment
    )

-- | Client for "Benchmarking API".
getHistory
    :: Maybe (CId Wal)
    -> Maybe CAccountId
    -> Maybe (CId Addr)
    -> Maybe ScrollOffset
    -> Maybe ScrollLimit
    -> ClientM ([CTx], Word)
getWallets
    :: ClientM [CWallet]
newPayment
    :: Maybe CPassPhrase
    -> CAccountId
    -> CId Addr
    -> Coin
    -> Maybe InputSelectionPolicy
    -> ClientM CTx
getHistory :<|> getWallets :<|> newPayment = client (Proxy @WalletBenchApi)
