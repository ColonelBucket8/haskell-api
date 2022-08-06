module Main where

import Control.Monad.IO.Class
import qualified Db
import User (CreateUserRequest (..))
import Web.Scotty

main :: IO ()
main = do
    db <- Db.mkDb

    scotty 8080 $ do

        post "/users" $ do
            createUserReq <- jsonData

            newUserId <- liftIO $ createUser db createUserReq

            json newUserId

        delete "/users/:userId" $ do

            userId <- param "userId"

            liftIO $ Db.deleteUser db userId

createUser :: Db.UserStore -> CreateUserRequest -> IO Int
createUser db CreateUserRequest { name = uname, password = pwd } = Db.insertUser db dbusr
    where
        dbusr = Db.DbUsr { Db.dbUsrName = uname, Db.dbUsrPassword = pwd }

