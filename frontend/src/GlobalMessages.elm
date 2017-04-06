module GlobalMessages exposing (Msg(..))

import ServerApi exposing (Jwt)

type Msg
    = None
    | SaveJwt Jwt
    | RemoveJwt

