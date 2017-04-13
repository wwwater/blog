module Styles           exposing (..)

import Html             exposing (..)
import Html.Attributes  exposing ( style )


iconStyle : Attribute msg
iconStyle = style [
      ("color", "#fff")
    , ("font-size", "14px")
    , ("margin-left", "24px")
    , ("cursor", "pointer")
    , ("box-shadow", "2px 2px 15px 1px #111")
    , ("padding", "10px")
    , ("border-radius", "20px")
    , ("background", "#555")
    ]

postStyle : Attribute msg
postStyle = style [
      ("display", "flex")
    , ("flex-direction", "column")
    , ("margin", "32px")
    , ("padding", "32px")
    , ("width", "800px")
    , ("max-width", "calc(100vw - 64px)")
    , ("box-shadow", "2px 2px 15px 1px #333")
    , ("border-radius", "2px")
    , ("background-color", "#333")
    ]

formStyle : Attribute msg
formStyle = style [
    ("box-shadow", "5px 5px 10px 1px #333")
    , ("padding", "8px")
    , ("border-radius", "3px")
    , ("border", "none")
    ]
