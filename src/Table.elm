module Table exposing (table)

import Element exposing (Element, centerX, column, el, fill, fillPortion, height, none, padding, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font


table =
    row [ spacing 16, width fill, centerX ]
        [ tableColumn F F S S "P"
        , tableColumn F S F S "Q"
        , verticalSeparator
        , tableColumn F F F F "c0" --0
        , tableColumn S F F F "nand" --1
        , tableColumn F S F F "?" --2
        , tableColumn S S F F "?" --3
        , tableColumn F F S F "?" --4
        , tableColumn S F S F "?" --5
        , tableColumn F S S F "?" --6
        , tableColumn S S S F "?" --7
        , tableColumn F F F S "?" --8
        , tableColumn S F F S "?" --9
        , tableColumn F S F S "?" --10
        , tableColumn S S F S "?" --11
        , tableColumn F F S S "?" --12
        , tableColumn S F S S "?" --13
        , tableColumn F S S S "?" --14
        , tableColumn S S S S "c1" --15
        ]


verticalSeparator =
    el [ height fill, padding 1, Background.color (rgb 0 0 0) ] none


type EntenEller
    = S
    | F


entenEllerString : EntenEller -> String
entenEllerString ee =
    case ee of
        S ->
            "1"

        F ->
            "0"


entenEllerElement : EntenEller -> Element msg
entenEllerElement ee =
    text (entenEllerString ee) |> el [ centerX ]


tableColumn : EntenEller -> EntenEller -> EntenEller -> EntenEller -> String -> Element msg
tableColumn t1 t2 t3 t4 head =
    column [ spacing 8, centerX, width (fillPortion 1) ] <| el [ Font.bold ] (text head) :: List.map entenEllerElement [ t1, t2, t3, t4 ]
