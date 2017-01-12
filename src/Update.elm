module Update exposing (..)

import Model exposing (..)
import Array exposing (Array)
import Random
import RandomConway exposing (randomGrid)


initialGrid : Grid
initialGrid =
    Array.initialize n (\i -> Array.initialize n (\j -> (Cell False i j)))


initialModel : Model
initialModel =
    { grid = initialGrid
    , runningPeriodical = False
    }


type Msg
    = Switch Cell
    | Step
    | StepPeriodical
    | Run
    | Stop
    | GenerateRandomGrid
    | SetRandomGrid Grid
    | Clear


init =
    ( initialModel, generateRandomGrid )


updateCell : Grid -> Cell -> Cell
updateCell grid cell =
    let
        neighbours =
            getNeighbours cell grid

        liveNeighbours =
            List.length <| List.filter .active neighbours

        newState =
            if cell.active then
                liveNeighbours == 2 || liveNeighbours == 3
            else
                liveNeighbours == 3
    in
        { cell | active = newState }


step : Grid -> Grid
step grid =
    Array.map (Array.map (updateCell grid)) grid


executeStep : Model -> Model
executeStep model =
    { model | grid = (step model.grid) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Switch { active, x, y } ->
            let
                rowM =
                    Array.get x model.grid

                updatedRowM =
                    Maybe.map (Array.set y (Cell (not active) x y)) rowM

                newGridM =
                    Maybe.map (\newRow -> Array.set x newRow model.grid) updatedRowM

                newGrid =
                    (Maybe.withDefault model.grid newGridM)
            in
                { model | grid = newGrid } ! []

        Step ->
            executeStep model ! []

        StepPeriodical ->
            (if model.runningPeriodical then
                executeStep model
             else
                model
            )
                ! []

        Run ->
            { model | runningPeriodical = True } ! []

        Stop ->
            { model | runningPeriodical = False } ! []

        GenerateRandomGrid ->
            model ! [ generateRandomGrid ]

        SetRandomGrid randGrid ->
            { model | grid = randGrid } ! []

        Clear ->
            initialModel ! []


generateRandomGrid : Cmd Msg
generateRandomGrid =
    Random.generate SetRandomGrid (randomGrid n)
