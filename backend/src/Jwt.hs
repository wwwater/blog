{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE FlexibleInstances #-}
module Jwt where

import Web.JWT                  (iat, exp, secret, encodeSigned, Algorithm(HS256), def, JSON, intDate)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import Control.Monad.IO.Class   (liftIO)
import Data.Text                (pack)

createJwt :: String -> IO JSON
createJwt key = do
    now <- getPOSIXTime
    let expire = now + 60 * 60
        claimsSet = def { iat = intDate now, Web.JWT.exp = intDate expire }
     in return $ encodeSigned HS256 (secret (pack key)) claimsSet

