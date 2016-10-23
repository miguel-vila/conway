module Styles exposing (..)

import Css exposing (..)


cellSize =
    20


cellStyle : Color -> List Mixin
cellStyle color =
    [ position relative
    , backgroundColor color
    , float left
    , width (px cellSize)
    , height (px cellSize)
    ]


activeColor =
    hex "ca5d46"


activeStyles =
    cellStyle activeColor


inactiveColor =
    hex "8277cc"


inactiveStyles =
    cellStyle inactiveColor
