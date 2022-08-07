module Main where

import Control.Monad.IO.Class
import qualified Db
import User (CreateUserRequest (..))
import Web.Scotty
import Data.Monoid (mconcat)

main :: IO ()
main = do
    db <- Db.mkDb

    scotty 8080 $ do

        get "/users/:name" $ do
            userName <- param "name"
            html $ mconcat ["<h1>Hi, ", userName, "!</h1>"]  

        post "/users" $ do
            createUserReq <- jsonData

            newUserId <- liftIO $ createUser db createUserReq

            json newUserId

            -- IO operations like getLine, putStrLn only work inside
            -- the IO monad. Using them inside any other monad
            -- will trigger a type error
            liftIO . print $ "User with id " ++ show newUserId ++ " successfully created" 

        delete "/users/:userId" $ do

            userId <- param "userId"

            liftIO $ Db.deleteUser db userId

            liftIO . print $ "User with id " ++ show userId ++ " successfully removed from the system"

createUser :: Db.UserStore -> CreateUserRequest -> IO Int
createUser db CreateUserRequest { name = uname, password = pwd } = Db.insertUser db dbusr
    where
        dbusr = Db.DbUsr { Db.dbUsrName = uname, Db.dbUsrPassword = pwd }

