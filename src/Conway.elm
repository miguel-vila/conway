port module Conway exposing (..)

import Array exposing (Array)
import Time exposing (every, second)
import Css
import Update exposing (..)
import Html.App as Html
import View exposing (view)
import Model exposing (Model)


main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    every (second / 3) (always StepPeriodical)
