module Toggle exposing (main)

import Browser exposing (sandbox)
import Html exposing (..)
import Html.Events exposing (onClick)
import Toggleable exposing (Toggleable(..))


type alias Model =
    { menuItems : List (Toggleable MenuItem) }


type alias MenuItem =
    { name : String, details : String }


items : List (Toggleable MenuItem)
items =
    [ Open <| MenuItem "First" "A lot of content..."
    , Closed <| MenuItem "Second" "More long content..."
    ]


initial : Model
initial =
    Model items


type Msg
    = Toggle Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Toggle index ->
            { model
                | menuItems = List.indexedMap (toggleByIndex index) model.menuItems
            }


toggleByIndex : Int -> Int -> Toggleable a -> Toggleable a
toggleByIndex indexToToggle currentIndex toggleable =
    if indexToToggle == currentIndex then
        Toggleable.toggle toggleable

    else
        toggleable


view : Model -> Html Msg
view model =
    div [] <| List.indexedMap viewMenuItem model.menuItems


viewMenuItem : Int -> Toggleable MenuItem -> Html Msg
viewMenuItem index tg =
    case tg of
        Open mi ->
            openedItem index mi

        Closed mi ->
            closedItem index mi


openedItem : Int -> MenuItem -> Html Msg
openedItem index mi =
    div [ onClick <| Toggle index ]
        [ text "▼ ", text mi.name, p [] [ text mi.details ] ]


closedItem : Int -> MenuItem -> Html Msg
closedItem index mi =
    div [ onClick <| Toggle index ] [ text "► ", text mi.name ]


main =
    Browser.sandbox
        { init = initial
        , update = update
        , view = view
        }
