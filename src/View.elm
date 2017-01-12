module View exposing (view)

import Html exposing (Html, div, td, tr, tbody, table, text, button)
import Html.App as Html
import Html.Attributes exposing (style)
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


centerCssAttrs =
    [ ( "margin", "0 auto" ) ]


cellsView : Cells -> Html Msg
cellsView cells =
    let
        getCellColor =
            getColor cells

        tableCell cell =
            td [] [ cellView (getCellColor cell) cell ]

        rowView row =
            tr [] (List.map tableCell row)

        tableRows =
            List.map (Array.toList >> rowView) (Array.toList cells)
    in
        table [ style centerCssAttrs ] [ tbody [] tableRows ]


playPauseButt : Bool -> Html Msg
playPauseButt running =
    if running then
        button [ onClick Stop ] [ text "⏸" ]
    else
        button [ onClick Run ] [ text "▶️" ]


view : Model -> Html Msg
view model =
    div []
        [ cellsView model.cells
        , div [ style centerCssAttrs ]
            [ button [ onClick Step ] [ text "Step!" ]
            , playPauseButt model.runningPeriodical
            , button [ onClick GenerateRandom ] [ text "Generate Random!" ]
            , button [ onClick Clear ] [ text "Clear" ]
            ]
        ]


styles =
    Css.asPairs >> Html.Attributes.style



{-
   This may be inefficient?
-}


getColor : Cells -> Cell -> Css.Color
getColor cells =
    let
        regions =
            getRegions cells

        regionsColors =
            getRegionsColors regions

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
