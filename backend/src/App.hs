{-# LANGUAGE OverloadedStrings #-}
module App (app) where


import qualified Api
import Servant.Server                 (serve, Application)
import Database.SQLite.Simple as Sql


app :: Sql.Connection -> Application
app conn = serve Api.api (Api.combinedServer conn)


