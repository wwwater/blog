{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Control.Exception (bracket)
import Database.SQLite.Simple as Sql

import qualified App
import qualified Storage


testConnect :: IO Sql.Connection
-- testConnect = Sql.open ":memory:"
testConnect = Sql.open "db/blog.db"


withTestConnection :: (Sql.Connection -> IO a) -> IO a
withTestConnection cb =
  withConn $ \conn -> cb conn
  where
    withConn = bracket testConnect Sql.close


blogCors :: Middleware
blogCors = cors $ const (Just blogResourcePolicy)


blogResourcePolicy :: CorsResourcePolicy
blogResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"]
        , corsRequestHeaders = "jwt":simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }



main :: IO ()
main = do
  withTestConnection $ \conn ->  do
    Storage.createSchema conn
    run 8081 $ logStdoutDev $ blogCors $ App.app conn
