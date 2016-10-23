module CellPatterns exposing (..)

import Css exposing (..)
import List.Nonempty
import List.Extra exposing (find)
import Region exposing (..)


type alias Pattern =
    List ( Int, Int )


squarePattern : Pattern
squarePattern =
    [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ]


singleDot : Pattern
singleDot =
    [ ( 0, 0 ) ]


horizontalLine : Pattern
horizontalLine =
    [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ]


verticalLine : Pattern
verticalLine =
    [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]


beehive : Pattern
beehive =
    [ ( 1, 0 ), ( 0, 1 ), ( 2, 1 ), ( 0, 2 ), ( 2, 2 ), ( 1, 3 ) ]


boat : Pattern
boat =
    [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 2 ), ( 2, 1 ) ]


hasPattern : Pattern -> Region -> Bool
hasPattern pattern region =
    let
        c0 =
            List.Nonempty.head region

        normalized =
            region
                |> List.Nonempty.map (\{ x, y } -> ( x - c0.x, y - c0.y ))
                |> List.Nonempty.toList
    in
        pattern == normalized


(=>) =
    (,)


patternsAndColors : List ( Pattern, Color )
patternsAndColors =
    [ squarePattern => hex "d97e00"
    , singleDot => hex "ff65a6"
    , horizontalLine => hex "38c447"
    , verticalLine => hex "00b487"
    , beehive => hex "817700"
    , boat => hex "b88c64"
    ]


getRegionColor : Region -> Maybe Color
getRegionColor region =
    find (\( pattern, _ ) -> hasPattern pattern region) patternsAndColors
        |> Maybe.map (\( _, color ) -> color)


getRegionsColors : List Region -> List ( Region, Color )
getRegionsColors =
    List.filterMap
        (\region ->
            getRegionColor region
                |> Maybe.map (\color -> region => color)
        )
