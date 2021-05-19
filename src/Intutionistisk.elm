module Intutionistisk exposing (..)

import Browser
import Debug exposing (toString)
import Element exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, explain, fill, fillPortion, height, none, padding, rgb, rgb255, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html, div, p)
import Html.Attributes as HA
import Markdown.Option exposing (..)
import Markdown.Render exposing (MarkdownMsg(..), MarkdownOutput(..))



--- MODEL ---


type alias JudgmentTrue =
    { prop : Prop
    , evidence : JudgmentTrueEvidence
    }


type JudgmentTrueEvidence
    = Assumption
    | Hypothetical Variable Prop JudgmentTrue
    | Inferred (List JudgmentTrue)


type Variable
    = A
    | B
    | C
    | D
    | H
    | Absurdity


type Prop
    = Atom Variable
    | New PropConnective Prop Prop


type PropConnective
    = And
    | Or
    | Implies
    | Unary


assumeTrue : Prop -> JudgmentTrue
assumeTrue prop =
    { prop = prop, evidence = Assumption }


introduce : Prop -> JudgmentTrue -> JudgmentTrue
introduce new old =
    { prop = new, evidence = Inferred [ old ] }


hypTrue : Variable -> Prop -> JudgmentTrue
hypTrue var prop =
    { prop = prop, evidence = Assumption }


connective : Prop -> PropConnective
connective prop =
    case prop of
        Atom var ->
            Unary

        New conn p1 p2 ->
            conn


introAnd : JudgmentTrue -> JudgmentTrue -> JudgmentTrue
introAnd a b =
    JudgmentTrue (New And a.prop b.prop)
        (Inferred
            [ a, b ]
        )


elimAndLeft : JudgmentTrue -> JudgmentTrue
elimAndLeft judgment =
    case judgment.prop of
        New And a b ->
            JudgmentTrue a (Inferred [ judgment ])

        _ ->
            Debug.todo "Error elimAndLeft"


elimAndRight : JudgmentTrue -> JudgmentTrue
elimAndRight judgment =
    case judgment.prop of
        New And a b ->
            JudgmentTrue b (Inferred [ judgment ])

        _ ->
            Debug.todo "Error elimAndRight"


introOrLeft : JudgmentTrue -> Prop -> JudgmentTrue
introOrLeft judgment injectedProp =
    JudgmentTrue (New Or judgment.prop injectedProp) (Inferred [ judgment ])


introOrRight : Prop -> JudgmentTrue -> JudgmentTrue
introOrRight injectedProp judgment =
    JudgmentTrue (New Or injectedProp judgment.prop) (Inferred [ judgment ])


introImplication : Variable -> Prop -> JudgmentTrue -> JudgmentTrue
introImplication var prop jud =
    { prop = New Implies prop jud.prop, evidence = Hypothetical var prop jud }

elimImplication : JudgmentTrue -> JudgmentTrue -> JudgmentTrue
elimImplication prem impl =
    case impl.prop of 
        New Implies a b -> JudgmentTrue b (Inferred [prem, impl])

        _ -> Debug.todo "Error elimImplication"

elimAbsurd : JudgmentTrue -> Prop -> JudgmentTrue
elimAbsurd absurd prop =
    JudgmentTrue prop (Inferred [absurd])

--a_and_b : JudgmentTrue
--a_and_b =
--    assumeTrue (New And (Atom A) (Atom B))


b_and_a_from_a_and_b : JudgmentTrue -> JudgmentTrue
b_and_a_from_a_and_b a_and_b =
    --JudgmentTrue (New And (Atom B) (Atom A)) <|
    introAnd (a_and_b |> elimAndRight) (a_and_b |> elimAndLeft)


b_or_a_from_a_and_b : JudgmentTrue -> JudgmentTrue
b_or_a_from_a_and_b a_and_b =
    introOrLeft (elimAndRight a_and_b) (Atom A)


transitive_implication : Prop -> Prop -> Prop -> JudgmentTrue
transitive_implication p q r =
    let -- and we assume p in hypothetical, extra hardcode for now.
        given1 = assumeTrue (New Implies p q)
        given2 = assumeTrue (New Implies q r) 
    in
    introImplication H p <| elimImplication (elimImplication (assumeTrue p) given1) given2
    
    --assumeTrue (New Implies p q)
    --assumeTrue (New Implies q r)

--- VIEW ---


drawExample =
    column [ spacing 64 ]
        [ text "konjunktion kommuterer"
        , drawJudgment <| b_and_a_from_a_and_b <| assumeTrue (New And (Atom A) (Atom B))
        , drawJudgment <| b_and_a_from_a_and_b <| assumeTrue (New And (Atom D) (Atom C))
        , text "fra konjunktion følger disjunktion"
        , drawJudgment <| b_or_a_from_a_and_b <| assumeTrue (New And (Atom A) (Atom B))
        , text "implikation er transitivt"
        , drawJudgment <| transitive_implication (Atom A) (Atom B) (Atom C)
        , text "korollar: n(P og nP)"
        ]



--- DRAW HELPERS ---


drawJudgment judgment =
    case judgment.evidence of
        Assumption ->
            drawProp judgment.prop

        Hypothetical hyp _ exhibit ->
            column [ centerX, width shrink ]
                (drawHypothesis hyp
                    ++ [ drawJudgment exhibit
                       , separator
                       , drawProp judgment.prop
                       ]
                )

        Inferred exhibits ->
            column [ centerX ]
                [ row [ spacing 32, centerX ] (List.map drawJudgment exhibits)
                , separator
                , drawProp judgment.prop
                ]


drawHypothesis hyp =
    [ el [ centerX, Font.color (rgb (1 - 0.666) (1 - 0.666) (1 - 0.666)) ] <|
        text
            (case hyp of
                A ->
                    "a"

                B ->
                    "b"

                C ->
                    "c"

                D ->
                    "d"

                H ->
                    "h"
                _ ->
                    Debug.todo "Error drawHypothesis"
            )
    , separator
    ]



{- column [ centerX, alignBottom ]
   ([ row [ width fill, spacing 32 ]
       (List.map drawProp judgment.premises
           ++ List.map drawHDerivation rule.hDerivations
       )
    ]
       ++ [ separator ]
       ++ [ row [ width fill, height fill, spacing 32 ] (List.map drawProp rule.conclusions) ]
   )
-}


separator =
    el [ width fill, padding 1, Background.color (rgb255 0 0 0) ] none


atomString : Variable -> String
atomString atom =
    case atom of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        D ->
            "D"

        Absurdity -> "absurd"

        _ ->
            Debug.todo "Error"


symbol : PropConnective -> String
symbol con =
    case con of
        And ->
            "∧"

        Or ->
            "∨"

        Implies ->
            "⊃"

        Unary ->
            "Bugsy"


propString : Prop -> String
propString prop =
    case prop of
        Atom atom ->
            atomString atom

        New con left right ->
            "(" ++ propString left ++ symbol con ++ propString right ++ ")"


drawProp : Prop -> Element msg
drawProp prop =
    el [ centerX, alignBottom ] (propString prop |> Element.text)
