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
    , cursor pointer
    ]


activeColor =
    hex "7A7A7A"


activeStyles =
    cellStyle activeColor


inactiveColor =
    hex "CCCCCC"


inactiveStyles =
    cellStyle inactiveColor


buttonStyle =
    [ margin (px 5) ]
