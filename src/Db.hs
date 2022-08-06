module Db
    ( DbUsr (..),
      getUserStore,
      insertUser,
      deleteUser,
      mkDb,
      UserStore (..),
    )
where

import Data.IORef
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

data DbUsr = DbUsr
    { dbUsrName :: Text, 
      dbUsrPassword :: Text 
    }
    deriving (Show)
    
newtype UserStore = UserStore { unUsrStore :: IORef (Map Int DbUsr) }

mkDb :: IO UserStore
mkDb = do
    ref <- newIORef (Map.empty :: Map Int DbUsr)
    pure $ UserStore ref

safeLast :: a -> [a] -> a
safeLast x [] = x
safeLast _ (x:xs) = safeLast x xs

getUserStore :: UserStore -> IO (Map Int DbUsr)
getUserStore (UserStore store) = readIORef store

getNextId :: UserStore -> IO Int
getNextId x = (+ 1) . safeLast 0 . sort . Map.keys <$> getUserStore x

insertUser :: UserStore -> DbUsr -> IO Int
insertUser userStore usr = do
    nextId <- getNextId userStore
    modifyIORef (unUsrStore userStore) (Map.insert nextId usr)
    pure nextId

deleteUser :: UserStore -> Int -> IO ()
deleteUser usrStore uid = modifyIORef' (unUsrStore usrStore) (Map.delete uid)