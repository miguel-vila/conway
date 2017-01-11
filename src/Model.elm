module Model exposing (..)

import Array exposing (Array)


-- Number of rows and columns


n : Int
n =
    25


type alias Cell =
    { active : Bool
    , x : Int
    , y : Int
    }


type alias Cells =
    Array (Array Cell)


type alias Model =
    { cells : Cells
    , runningPeriodical : Bool
    }


getCellAt : Cells -> ( Int, Int ) -> Maybe Cell
getCellAt cells ( x, y ) =
    let
        row =
            (Array.get x cells)
    in
        Maybe.andThen row (Array.get y)


neighbours : Cell -> List ( Int, Int )
neighbours { x, y } =
    let
        isValid ( i, j ) =
            0 <= i && i < n && 0 <= j && j < n

        range =
            [ -1, 0, 1 ]

        positions =
            List.concatMap (\i -> List.map (\j -> ( x + i, y + j )) range) range
    in
        positions
            |> List.filter isValid
            |> List.filter (\( i, j ) -> x /= i || y /= j)


getNeighbours : Cell -> Cells -> List Cell
getNeighbours cell cells =
    neighbours cell
        |> List.filterMap (getCellAt cells)
