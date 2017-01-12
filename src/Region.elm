module Region exposing (..)

import Model exposing (..)
import Array exposing (Array)
import Cons exposing (Cons)


type alias Visited =
    Array (Array Bool)


type alias Region =
    Cons Cell


getCellRegion : Visited -> Grid -> Cell -> ( Visited, List Cell )
getCellRegion initialVisited grid initialCell =
    let
        initialVisited' =
            markCellAsVisited initialCell initialVisited

        nonVisitedNeighbours visited cell =
            getNeighbours cell grid
                |> List.filter (\c -> not <| visitedPreviously visited c)
                |> List.filter .active

        loop queue visited acc =
            case queue of
                [] ->
                    ( visited, acc )

                x :: xs ->
                    let
                        neighbours =
                            nonVisitedNeighbours visited x

                        newVisited =
                            markAsVisited visited neighbours

                        newQueue =
                            neighbours ++ xs
                    in
                        loop newQueue newVisited (x :: acc)
    in
        loop [ initialCell ] initialVisited' []


compareCell : Cell -> Cell -> Order
compareCell cell1 cell2 =
    if cell1.x < cell2.x then
        LT
    else if cell1.x == cell2.x then
        if cell1.y < cell2.y then
            LT
        else if cell1.y == cell2.y then
            EQ
        else
            GT
    else
        GT


visitedPreviously : Visited -> Cell -> Bool
visitedPreviously visited { x, y } =
    Maybe.andThen (Array.get x visited) (Array.get y)
        |> Maybe.withDefault False


markCellAsVisited : Cell -> Visited -> Visited
markCellAsVisited { x, y } visited =
    Array.get x visited
        |> Maybe.map (\row -> Array.set y True row)
        |> Maybe.map (\newRow -> Array.set x newRow visited)
        |> Maybe.withDefault visited


markAsVisited : Visited -> List Cell -> Visited
markAsVisited visited grid =
    List.foldl markCellAsVisited visited grid


getRegions : Grid -> List Region
getRegions grid =
    let
        initialVisited =
            Array.repeat n (Array.repeat n False)

        rowRegions row ( visited, regions ) =
            Array.foldl
                (\cell ( visited, regions ) ->
                    if cell.active && not (visitedPreviously visited cell) then
                        let
                            ( visited', region ) =
                                getCellRegion visited grid cell

                            regionAsNonempty =
                                Cons.fromList (List.sortWith compareCell region)

                            newRegions =
                                regionAsNonempty
                                    |> Maybe.map (\region -> region :: regions)
                                    |> Maybe.withDefault regions
                        in
                            ( visited', newRegions )
                    else
                        ( visited, regions )
                )
                ( visited, regions )
                row

        ( _, regions ) =
            Array.foldl rowRegions ( initialVisited, [] ) grid
    in
        regions
