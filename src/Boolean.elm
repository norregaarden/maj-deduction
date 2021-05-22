module Boolean exposing (..)

import Element exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, explain, fill, fillPortion, height, none, padding, rgb, rgb255, row, shrink, spacing, text, width)
import Katex as K
import String exposing (fromInt)


binaryConnective : String -> Int -> Int -> Int -> Int -> Element msg
binaryConnective navn nul en to tre =
    column [ spacing 16, width fill ]
        [ row [ width fill, spacing 16, width fill ]
            [ navn |> K.inline |> K.print |> text |> el [ alignLeft ]
            , "0" |> K.inline |> K.print |> text |> el [ centerX ]
            , "1" |> K.inline |> K.print |> text |> el [ alignRight ]
            ]
        , row [ width fill, spacing 16, width fill ]
            [ "0" |> K.inline |> K.print |> text |> el [ alignLeft ]
            , nul |> fromInt |> K.inline |> K.print |> text |> el [ centerX ]
            , en |> fromInt |> K.inline |> K.print |> text |> el [ alignRight ]
            ]
        , row [ width fill, spacing 16, width fill ]
            [ "1" |> K.inline |> K.print |> text |> el [ alignLeft ]
            , to |> fromInt |> K.inline |> K.print |> text |> el [ centerX ]
            , tre |> fromInt |> K.inline |> K.print |> text |> el [ alignRight ]
            ]
        ]


cartesianProduct : List String -> Element msg
cartesianProduct domain =
    column [ spacing 16, width fill ] <|
        row [ width fill, spacing 16 ] ((text "" |> el [ width <| fillPortion 1 ]) :: List.map (\y -> text ("y" ++ y) |> el [ width <| fillPortion 1 ]) domain)
            :: List.map (\x -> row [ spacing 16 ] <| text ("x" ++ x) :: List.map (\y -> el [] <| text <| x ++ y) domain) domain


binaryOperation : String -> List String -> Element msg
binaryOperation name domain =
    let
        cardinality =
            List.length domain
    in
    column [ spacing 16, width fill ] <|
        row [ width fill, spacing 16 ] ((text name |> el [ width <| fillPortion 1 ]) :: List.map (\y -> text ("y" ++ y) |> el [ width <| fillPortion 1 ]) domain)
            :: List.map (\x -> row [ spacing 16 ] <| text ("x" ++ x) :: List.map (\y -> el [] <| text <| x ++ y) domain) domain



-- table : List String -> Element msg
-- table =
--     let
--         cardinality = 2
--         ary = 2
--         foldl (\a b -> () :: b) []
--     in
