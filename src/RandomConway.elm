module RandomConway exposing (randomCells)

import Random exposing (Generator)
import Random.Extra
import Model exposing (..)
import Array exposing (Array)


randomCell : Int -> Int -> Generator Cell
randomCell x y =
    Random.map (\active -> Cell active x y) Random.bool


randomRow : Int -> Int -> Generator (Array Cell)
randomRow n x =
    randomCell x
        |> List.repeat n
        |> List.indexedMap (|>)
        |> Random.Extra.together
        |> Random.map Array.fromList


randomCells : Int -> Generator Cells
randomCells n =
    (randomRow n)
        |> List.repeat n
        |> List.indexedMap (|>)
        |> Random.Extra.together
        |> Random.map Array.fromList
