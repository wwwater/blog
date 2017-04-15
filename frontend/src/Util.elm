module Util             exposing ( formatCreationDate, renderPostContent )

import Date             exposing ( fromTime, day, month, year )
import Regex            exposing ( Regex (..)
                                 , regex
                                 , split
                                 , find
                                 , HowMany (..)
                                 )
import Html             exposing ( Html, text, a )
import Html.Attributes  exposing ( href, target )


formatCreationDate : Maybe Int -> String
formatCreationDate createdAt =
    case createdAt of
        Just unixTime ->
            let date = fromTime (toFloat unixTime * 1000)
                d = toString <| day date
                m = toString <| month date
                y = toString <| year date
            in d ++ " " ++ m ++ " " ++ y
        Nothing -> "Unknown posting date"


urlRegex : Regex
urlRegex = regex
    "https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[-a-zA-Z0-9@:%_\\+.~#?&//=]{2,}"

mixLists : List a -> List a -> List a -> List a
mixLists a b result =
    if List.isEmpty a && List.isEmpty b
    then result
    else
        let
            heads = List.append (List.take 1 a) (List.take 1 b)
            newResult = List.append result heads
        in mixLists (List.drop 1 a) (List.drop 1 b) newResult

renderPostContent : String -> List (Html a)
renderPostContent content =
    let
        textPieces = split All urlRegex content
        urlPieces = List.map .match <| find All urlRegex content
        texts = List.map text textPieces
        urls = List.map
            (\url ->
                a [ href url
                  , target "_blank" ]
                    [ text "link" ]
            )
            urlPieces
    in
        mixLists texts urls []
