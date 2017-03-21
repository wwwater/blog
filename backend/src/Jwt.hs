{-#LANGUAGE OverloadedStrings #-}

module Jwt where

import Web.JWT                  (iat,
                                exp,
                                secret,
                                encodeSigned,
                                decodeAndVerifySignature,
                                Algorithm(HS256),
                                def,
                                JSON,
                                intDate,
                                claims,
                                secondsSinceEpoch)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import Data.Text                (pack)

createJwt :: String -> IO JSON
createJwt key = do
  now <- getPOSIXTime
  let expire = now + 60 * 60
      claimsSet = def { iat = intDate now, Web.JWT.exp = intDate expire }
   in return $ encodeSigned HS256 (secret (pack key)) claimsSet

verifyJwt :: String -> String -> IO Bool
verifyJwt key jwt =
  case decodeAndVerifySignature (secret (pack key)) (pack jwt) of
    Just verifiedJwt ->
      case Web.JWT.exp $ claims verifiedJwt of
        Just expiredTime -> do
          now <- getPOSIXTime
          let expire = secondsSinceEpoch expiredTime
            in if now > expire then return False else return True
        Nothing -> return False
    Nothing -> return False

