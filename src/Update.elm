module Update exposing (..)

import Model exposing (..)
import Array exposing (Array)
import Random
import RandomConway exposing (randomCells)


initialCells : Cells
initialCells =
    Array.initialize n (\i -> Array.initialize n (\j -> (Cell False i j)))


initialModel : Model
initialModel =
    { cells = initialCells
    , runningPeriodical = False
    }


type Msg
    = Switch Cell
    | Step
    | StepPeriodical
    | Run
    | Stop
    | GenerateRandomCells
    | SetRandomCells Cells
    | Clear


init =
    ( initialModel, generateRandomCells )


updateCell : Cells -> Cell -> Cell
updateCell cells cell =
    let
        neighbours =
            getNeighbours cell cells

        liveNeighbours =
            List.length <| List.filter .active neighbours

        newState =
            if cell.active then
                liveNeighbours == 2 || liveNeighbours == 3
            else
                liveNeighbours == 3
    in
        { cell | active = newState }


step : Cells -> Cells
step cells =
    Array.map (Array.map (updateCell cells)) cells


executeStep : Model -> Model
executeStep model =
    { model | cells = (step model.cells) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Switch { active, x, y } ->
            let
                rowM =
                    Array.get x model.cells

                updatedRowM =
                    Maybe.map (Array.set y (Cell (not active) x y)) rowM

                newCellsM =
                    Maybe.map (\newRow -> Array.set x newRow model.cells) updatedRowM

                newCells =
                    (Maybe.withDefault model.cells newCellsM)
            in
                { model | cells = newCells } ! []

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

        GenerateRandomCells ->
            model ! [ generateRandomCells ]

        SetRandomCells randCells ->
            { model | cells = randCells } ! []

        Clear ->
            initialModel ! []


generateRandomCells : Cmd Msg
generateRandomCells =
    Random.generate SetRandomCells (randomCells n)
