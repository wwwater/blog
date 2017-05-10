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
                                numericDate,
                                claims,
                                secondsSinceEpoch)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import Data.Text                (pack)

import Model                    as M



createJwt :: M.JwtSecret -> IO JSON
createJwt key = do
  now <- getPOSIXTime
  let expire = now + 60 * 60 * 24 -- 24 hours
      claimsSet = def { iat = numericDate now, Web.JWT.exp = numericDate expire }
   in return $ encodeSigned HS256 (secret (pack key)) claimsSet

verifyJwt :: M.JwtSecret -> M.JwtToken -> IO Bool
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

