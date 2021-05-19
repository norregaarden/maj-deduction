module Intro exposing (intro)

import Element exposing (html)
import Html as H exposing (Html)
import Katex as K
    exposing
        ( Latex
        , display
        , human
        , inline
        )


passage : List Latex
passage =
    [ display "Vrøvl"
    , human "We denote by "
    , inline "\\phi \\psi \\alpha"
    , human " the formula for which "
    , display "\\Gamma \\vDash \\phi"
    , display "1+1=2"
    ]


intro =
    let
        htmlGenerator isDisplayMode stringLatex =
            case isDisplayMode of
                Just True ->
                    H.div [] [ H.text stringLatex ]

                _ ->
                    H.span [] [ H.text stringLatex ]
    in
    passage
        |> List.map (K.generate htmlGenerator)
        |> H.div []
        |> Element.html
