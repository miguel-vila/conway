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


rotatedClockwiseOnce : Template -> Pattern
rotatedClockwiseOnce base =
    Cons.cons base [ rotateClockwise base ]


allRotations : Template -> Pattern
allRotations base =
    let
        rotations =
            Cons.cons rotateClockwise (List.repeat 2 rotateClockwise)
    in
        Cons.foldl
            (\rotate templates ->
                let
                    newTemplate =
                        rotate (Cons.head templates)
                in
                    Cons.cons newTemplate (Cons.toList templates)
            )
            (Cons.singleton base)
            rotations


square : Pattern
square =
    Cons.singleton <| Cons.cons ( 0, 0 ) [ ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ]


singleDot : Pattern
singleDot =
    Cons.singleton (Cons.singleton ( 0, 0 ))


pair : Pattern
pair =
    rotatedClockwiseOnce (Cons.cons ( 0, 0 ) [ ( 0, 1 ) ])


line : Pattern
line =
    rotatedClockwiseOnce <| Cons.cons ( 0, 0 ) [ ( 0, 1 ), ( 0, 2 ) ]


beehive : Pattern
beehive =
    rotatedClockwiseOnce <| Cons.cons ( 0, 1 ) [ ( 0, 2 ), ( 1, 0 ), ( 1, 3 ), ( 2, 1 ), ( 2, 2 ) ]


boat : Pattern
boat =
    allRotations <| Cons.cons ( 0, 0 ) [ ( 0, 1 ), ( 1, 0 ), ( 1, 2 ), ( 2, 1 ) ]


loaf : Pattern
loaf =
    allRotations <| Cons.cons ( 0, 1 ) [ ( 0, 2 ), ( 1, 0 ), ( 1, 3 ), ( 2, 1 ), ( 2, 3 ), ( 3, 2 ) ]


cross : Pattern
cross =
    Cons.singleton (Cons.cons ( 0, 1 ) [ ( 1, 0 ), ( 1, 2 ), ( 2, 1 ) ])


corner : Pattern
corner =
    allRotations <| Cons.cons ( 0, 0 ) [ ( 0, 1 ), ( 1, 0 ) ]


toad : Pattern
toad =
    let
        period1 =
            rotatedClockwiseOnce <| Cons.cons ( 0, 1 ) [ ( 0, 2 ), ( 0, 3 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]

        period2 =
            allRotations <| Cons.cons ( 0, 0 ) [ ( 1, 0 ), ( 2, 1 ) ]
    in
        Cons.append period1 period2


glider : Pattern
glider =
    let
        g1 =
            Cons.cons ( 0, 0 ) [ ( 1, 1 ), ( 1, 2 ), ( 2, 0 ), ( 2, 1 ) ]

        g2 =
            Cons.cons ( 0, 1 ) [ ( 1, 2 ), ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]

        g3 =
            Cons.cons ( 0, 0 ) [ ( 0, 2 ), ( 1, 1 ), ( 1, 2 ), ( 2, 1 ) ]

        g4 =
            Cons.cons ( 0, 2 ) [ ( 1, 0 ), ( 1, 2 ), ( 2, 1 ), ( 2, 2 ) ]

        gs =
            Cons.cons g1 [ g2, g3, g4 ]
    in
        Cons.concatMap allRotations gs


starExplosion : Pattern
starExplosion =
    let
        stage1 =
            Cons.cons ( 0, 2 ) [ ( 0, 3 ), ( 0, 4 ), ( 1, 1 ), ( 1, 5 ), ( 2, 0 ), ( 2, 6 ), ( 3, 1 ), ( 3, 5 ), ( 4, 2 ), ( 4, 3 ), ( 4, 4 ) ]

        stage2 =
            Cons.cons ( 0, 3 ) [ ( 1, 2 ), ( 1, 3 ), ( 1, 4 ), ( 2, 0 ), ( 2, 1 ), ( 2, 5 ), ( 2, 6 ), ( 3, 1 ), ( 3, 2 ), ( 3, 3 ), ( 3, 4 ), ( 3, 5 ), ( 4, 2 ), ( 4, 3 ), ( 4, 4 ), ( 5, 3 ) ]

        stage3 =
            Cons.cons ( 0, 3 ) [ ( 0, 4 ), ( 0, 5 ), ( 1, 1 ), ( 1, 5 ), ( 2, 0 ), ( 2, 6 ), ( 3, 0 ), ( 3, 6 ), ( 4, 0 ), ( 4, 6 ), ( 5, 1 ), ( 5, 5 ), ( 6, 2 ), ( 6, 3 ), ( 6, 4 ) ]
    in
        Cons.cons stage1 [ stage2, stage3 ]


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


colorPalette : List Color
colorPalette =
    List.map hex
        [ "e3342e"
        , "55b522"
        , "ac2ab2"
        , "1f7d00"
        , "ff83f0"
        , "d3ca31"
        , "02a8d0"
        , "a61012"
        , "6a9062"
        , "9d203c"
        , "5a6500"
        , "ff8eb1"
        , "84400a"
        , "f1bd74"
        , "8c4954"
        ]


patternsAndColors : List ( Pattern, Color )
patternsAndColors =
    List.Extra.zip
        [ beehive
        , square
        , singleDot
        , pair
        , line
        , boat
        , loaf
        , cross
        , corner
        , toad
        , glider
        , starExplosion
        ]
        colorPalette


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
