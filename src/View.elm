module View exposing (view)

import Html exposing (Html, div, td, tr, tbody, table, text, button)
import Html.App as Html
import Html.Attributes exposing (style, disabled)
import Styles exposing (..)
import Css
import Html.Events exposing (onClick)
import Model exposing (..)
import CellPatterns exposing (..)
import Region exposing (..)
import List.Extra exposing (find)
import Update exposing (..)
import Array
import Cons


cellView : Css.Color -> Cell -> Html Msg
cellView color cell =
    div
        [ styles (cellStyle color)
        , onClick (Switch cell)
        ]
        []


centerGridStyle =
    style [ ( "margin", "0 auto" ) ]


cellsView : Cells -> Html Msg
cellsView cells =
    let
        regions =
            getRegions cells

        regionsColors =
            getRegionsColors regions

        getCellColor =
            getColor regionsColors

        tableCell cell =
            td [] [ cellView (getCellColor cell) cell ]

        rowView row =
            tr [] (List.map tableCell row)

        tableRows =
            List.map (Array.toList >> rowView) (Array.toList cells)
    in
        table [ centerGridStyle ] [ tbody [] tableRows ]


conwayButton : Bool -> Msg -> String -> Html Msg
conwayButton isEnabled msg textString =
    button
        [ styles buttonStyle
        , onClick msg
        , disabled (not isEnabled)
        ]
        [ text textString ]


enabledButton =
    conwayButton True


playPauseButt : Bool -> Html Msg
playPauseButt running =
    if running then
        enabledButton Stop "⏸"
    else
        enabledButton Run "▶️"


buttons : Bool -> Html Msg
buttons running =
    div [ style [ ( "text-align", "center" ) ] ]
        [ conwayButton (not running) Step "One step"
        , playPauseButt running
        , enabledButton GenerateRandomCells "Generate Random"
        , enabledButton Clear "Clear"
        ]


view : Model -> Html Msg
view model =
    div []
        [ buttons model.runningPeriodical
        , cellsView model.cells
        ]


styles =
    Css.asPairs >> Html.Attributes.style


getColor : List ( Region, Css.Color ) -> Cell -> Css.Color
getColor regionsColors =
    let
        colorByRegion : Cell -> Maybe Css.Color
        colorByRegion cell =
            regionsColors
                |> find
                    (\( region, color ) ->
                        Cons.any (\regCell -> regCell == cell) region
                    )
                |> Maybe.map (\( _, color ) -> color)

        defaultColor : Cell -> Css.Color
        defaultColor cell =
            if cell.active then
                activeColor
            else
                inactiveColor

        cellColor cell =
            cell
                |> colorByRegion
                |> Maybe.withDefault (defaultColor cell)
    in
        cellColor
