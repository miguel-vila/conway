module CellPatterns exposing (..)

import Css exposing (..)
import List.Extra exposing (find)
import Region exposing (..)
import Debug
import Cons exposing (Cons)


type alias Template =
    Cons ( Int, Int )


type alias Pattern =
    Cons Template


fromRotated : Template -> Pattern
fromRotated base =
    Cons.cons base [ rotateClockwise base ]


square : Pattern
square =
    Cons.singleton <| Cons.cons ( 0, 0 ) [ ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ]


singleDot : Pattern
singleDot =
    Cons.singleton (Cons.singleton ( 0, 0 ))


line : Pattern
line =
    fromRotated <| Cons.cons ( 0, 0 ) [ ( 0, 1 ), ( 0, 2 ) ]


beehive : Pattern
beehive =
    fromRotated <| Cons.cons ( 0, 1 ) [ ( 0, 2 ), ( 1, 0 ), ( 1, 3 ), ( 2, 1 ), ( 2, 2 ) ]


boat : Pattern
boat =
    fromRotated <| Cons.cons ( 0, 0 ) [ ( 0, 1 ), ( 1, 0 ), ( 1, 2 ), ( 2, 1 ) ]


compare : ( Int, Int ) -> ( Int, Int ) -> Order
compare ( x1, y1 ) ( x2, y2 ) =
    if x1 == x2 then
        if y1 == y2 then
            EQ
        else if y1 < y2 then
            LT
        else
            GT
    else if x1 < x2 then
        LT
    else
        GT


rotateClockwise : Cons ( Int, Int ) -> Cons ( Int, Int )
rotateClockwise pattern =
    let
        maxX =
            pattern |> Cons.map fst |> Cons.maximum
    in
        pattern
            |> Cons.map (\( x, y ) -> ( y, maxX - x ))
            |> Cons.sortWith compare


matchesPattern : Pattern -> Region -> Bool
matchesPattern pattern region =
    let
        minx =
            region
                |> Cons.map .x
                |> Cons.minimum

        miny =
            region
                |> Cons.map .y
                |> Cons.minimum

        normalized =
            region
                |> Cons.map (\{ x, y } -> ( x - minx, y - miny ))
    in
        Cons.any (\p -> p == normalized) pattern


(=>) =
    (,)


patternsAndColors : List ( Pattern, Color )
patternsAndColors =
    [ beehive => hex "817700"
    , square => hex "d97e00"
    , singleDot => hex "ff65a6"
    , line => hex "38c447"
    , boat => hex "b88c64"
    ]


getRegionColor : Region -> Maybe Color
getRegionColor region =
    find (\( pattern, _ ) -> matchesPattern pattern region) patternsAndColors
        |> Maybe.map (\( _, color ) -> color)


getRegionsColors : List Region -> List ( Region, Color )
getRegionsColors =
    List.filterMap
        (\region ->
            getRegionColor region
                |> Maybe.map (\color -> region => color)
        )
