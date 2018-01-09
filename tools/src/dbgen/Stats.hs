{-# LANGUAGE TypeFamilies #-}
module Stats where

import           Prelude
import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict              as HM
import           Data.Monoid
import           Pos.Launcher.Configuration
import           Pos.Wallet.Web.State.Acidic
import           Pos.Wallet.Web.State.Storage
import           Rendering
import           Serokell.AcidState.ExtendedState
import           System.Exit
import           Text.Printf

{- For reference:

data WalletStorage = WalletStorage
    { _wsWalletInfos     :: !(HashMap (CId Wal) WalletInfo)
    , _wsAccountInfos    :: !(HashMap AccountId AccountInfo)
    , _wsProfile         :: !CProfile
    , _wsReadyUpdates    :: [CUpdateInfo]
    , _wsTxHistory       :: !(HashMap (CId Wal) (HashMap CTxId CTxMeta))
    , _wsHistoryCache    :: !(HashMap (CId Wal) (Map TxId TxHistoryEntry))
    , _wsUtxo            :: !Utxo
    -- @_wsBalances@ depends on @_wsUtxo@,
    -- it's forbidden to update @_wsBalances@ without @_wsUtxo@
    , _wsBalances        :: !WalletBalances
    , _wsUsedAddresses   :: !CustomAddresses
    , _wsChangeAddresses :: !CustomAddresses
    }
-}

showStatsAndExit :: HasConfigurations => FilePath -> IO ()
showStatsAndExit walletPath = do
    let db = walletPath
    bracket (openState False db) (\x -> closeState x >> exitSuccess) $ \db' -> do
        WalletStorage{..} <- getStorage db'
        let wallets  = HM.elems _wsWalletInfos
        let accounts = HM.toList _wsAccountInfos
        say $ bold "Wallets:" <> printf " %d"  (length wallets)
        listOf (map renderWallet wallets)
        blankLine
        say $ bold "Accounts:" <> printf " %d" (length accounts)
        listOf (map renderAccount accounts)
        say "\n"
        say $ printf "Number of used addresses: %d" (length _wsUsedAddresses)
        say $ printf "Number of change addresses: %d" (length _wsChangeAddresses)

-- TODO(ks): Yes, remove duplication...
showStatsNoExit :: HasConfigurations => FilePath -> IO ()
showStatsNoExit walletPath = do
    let db = walletPath
    bracket (openState False db) (\x -> closeState x) $ \db' -> do
        WalletStorage{..} <- getStorage db'
        let wallets  = HM.elems _wsWalletInfos
        let accounts = HM.toList _wsAccountInfos
        say $ bold "Wallets:" <> printf " %d"  (length wallets)
        listOf (map renderWallet wallets)
        blankLine
        say $ bold "Accounts:" <> printf " %d" (length accounts)
        listOf (map renderAccount accounts)
        say "\n"
        say $ printf "Number of used addresses: %d" (length _wsUsedAddresses)
        say $ printf "Number of change addresses: %d" (length _wsChangeAddresses)

showStatsData :: HasConfigurations => String -> FilePath -> IO ()
showStatsData mark walletPath = do
    blankLine
    say $ red $ "The stats " <> mark <> " modification:"
    showStatsNoExit walletPath
    blankLine

getStorage :: ExtendedState WalletStorage -> IO WalletStorage
getStorage db = liftIO (query db GetWalletStorage)

