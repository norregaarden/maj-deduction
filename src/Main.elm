module Main exposing (main)

import Boolean
import Browser
import Deduktion
import Element as E exposing (Element, column, el, fill, height, html, maximum, padding, paddingEach, paragraph, pointer, rgb, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Events as EEvents
import Element.Font as Font exposing (Font)
import Hello exposing (Msg)
import Html as H
import Html.Events exposing (onClick)
import Intro
import Intutionistisk as IPL
import Katex as K
    exposing
        ( Latex
        , display
        , human
        , inline
        )
import String exposing (fromInt)
import Table



--import Toggleable exposing (Toggleable(..))
{- view model =
   H.div [Html.Attributes.]
       [ text "Hej"
       , Intro.intro
       , Deduktion.reglerne
       ]
-}


indhold : List (Element Msg)
indhold =
    [ text ""
    , header "Hej"
    , paragraph []
        [ text "Vi vil i dag få et overblik over propositionel logik, både klassisk og intutionistisk, og som sidegevinst lære om funktionel, typesikker programmering."
        ]

    -- Klassisk logik: Syntax
    , text ""
    , header "Klassisk logik: Syntax"
    , para
        [ "Vi antager en mængde af atomare variabler "
        , texin "\\mathbb{V} = \\{ A,B,C, \\dots \\}."
        ]
    , para
        [ "Et velformet udtryk (WFF), "
        , texin "\\omega"
        , " eller "
        , texin "\\theta"
        , ", er bestemt rekursivt:"
        ]
    , texBlock """\\omega, \\theta 
        \\quad::=\\quad \\mathbb{V} 
        \\quad|\\quad \\omega \\wedge \\theta 
        \\quad|\\quad \\omega \\vee \\theta 
        \\quad|\\quad \\omega \\Rightarrow \\theta 
        \\quad|\\quad \\neg \\theta 
        """
    , footnote "Bla bla, noget binder implicit stærkere end andet; man kan visualisere det som et træ. Vi springer over: motivation, frie variable, variabel substitution, mm. Rekursivt == Strukturel induktion"

    -- Klassisk logik: Semantik
    , text ""
    , header "Klassisk logik: Semantik"
    , para
        [ "Vi definerer mængden af sandhedsværdier "
        , texin "\\mathbb{B} = \\{ 0,1 \\} = \\{ F, S \\} = \\{ \\bot, \\top \\}"
        , " med intentionen at dette skal give \"mening\"/semantik til udtrykkene."
        ]
    , para
        [ "I klassisk semantik tænker vi på operatorerne "
        , texin "\\wedge, \\vee, \\Rightarrow"
        , " som funktioner "
        , texin "\\sim: \\mathbb{B}^2 \\rightarrow \\mathbb{B}"
        , " og ligeledes "
        , texin "\\neg: \\mathbb{B} \\rightarrow \\mathbb{B}"
        ]
    , p "Vi definerer funktionerne gennem deres sandhedstabeller hvor kolonnen repræsenterer venstresiden og rækken repræsenterer højresiden:"
    , row [ spacing 64 ]
        [ Boolean.binaryConnective "\\wedge" 0 0 0 1
        , Boolean.binaryConnective "\\vee" 0 1 1 1
        , Boolean.binaryConnective "\\Rightarrow" 1 1 0 1
        ]
    , para
        [ "En fortolkning (interpretation/valuation/assignment) "
        , texin "\\rho"
        , " er enhver funktion "
        , texin "\\rho : \\mathbb{V} \\rightarrow \\mathbb{B} "
        ]
    , para
        [ "Semantikken "
        , texin "\\llbracket \\omega \\rrbracket_\\rho"
        , " af et udtryk  "
        , texin "\\omega"
        , " under fortolkningen "
        , texin "\\rho"
        , " er defineret rekursivt som:"
        ]
    , texBlock "\\llbracket \\omega \\rrbracket_\\rho = \\rho(\\omega)"
    , texBlock "\\llbracket \\omega \\wedge \\theta \\rrbracket_\\rho = \\llbracket \\omega \\rrbracket_\\rho \\wedge \\llbracket \\theta \\rrbracket_\\rho"
    , texBlock "\\llbracket \\omega \\vee \\theta \\rrbracket_\\rho = \\llbracket \\omega \\rrbracket_\\rho \\vee \\llbracket \\theta \\rrbracket_\\rho"
    , texBlock "\\llbracket \\omega \\Rightarrow \\theta \\rrbracket_\\rho = \\llbracket \\omega \\rrbracket_\\rho \\Rightarrow \\llbracket \\theta \\rrbracket_\\rho"
    , texBlock "\\llbracket \\neg \\omega \\rrbracket_\\rho = \\neg \\llbracket \\omega \\rrbracket_\\rho"
    , para
        [ "Vi siger at et udtryk "
        , texin "\\omega"
        , " er sandt under en fortolkning "
        , texin "\\rho"
        , " når "
        , texin "\\llbracket \\omega \\rrbracket_\\rho = 1"
        , " og falsk når "
        , texin "\\llbracket \\omega \\rrbracket_\\rho = 0"
        ]
    , para
        [ "(Semantic entailment) Vi siger at "
        , texin "\\rho"
        , " modellerer "
        , texin "\\omega"
        , " hviss "
        , texin "\\llbracket \\omega \\rrbracket_\\rho = 1"
        , " og skriver i så fald "
        , texin "\\rho \\models \\omega"
        ]
    , para
        [ "En mængde af udtryk "
        , texin "\\Gamma"
        , " modellerer "
        , texin "\\omega"
        , " hviss for alle "
        , texin "\\theta \\in \\Gamma"
        , ": Hvis "
        , texin "\\rho \\models \\theta"
        , " så "
        , texin "\\rho \\models \\omega"
        , ". Vi skriver i så fald "
        , texin "\\quad \\Gamma \\models \\omega"
        ]
    , para
        [ texin "\\omega"
        , " er gyldig (valid) hviss det er sandt under alle fortolkninger. Vi skriver i så fald "
        , texin "\\quad \\models \\omega \\quad"
        , "Vi siger også at "
        , texin "\\omega"
        , " er en tautologi."
        ]
    , para
        [ texin "\\omega"
        , " er opfyldelig (satisfiable) hviss der findes en fortolkning der modellerer den."
        ]

    -- Deduktion
    , text ""
    , header "Klassisk logik: Deduktion"

    --, header "Rice' Sætning"
    --, header "Church-Rosser Sætning"
    , header ""
    , header "Halting Problem"
    , header "Decidability"
    , header "Semantik"
    , header "Naturlig deduktion"
    , header "Historie"
    , header "Klassisk"
    , text "https://www.youtube.com/watch?v=HMoPqR4-jDg&t=68s&ab_channel=KyleBanick"
    , text "WFF -> Propositions, "
    , text """
The following can be seen as a form of completeness of the ¸-calculus.
1.5.20. Theorem (Kleene). All recursive functions are ¸-de¯nable.
Proof. By the above lemmas.
                """
    , text "nemt nok at vise at nor/nand er nok"
    , text "https://www.youtube.com/watch?v=xZY35QQf9kA&list=PLjJhPCaCziSRSUtQiTA_yx5TJ76G_EqUJ&index=6&ab_channel=AntonioMontalban"
    , text "Binary connectives are enough"
    , text "k-ary connectives in intutionistic logic has nice interpretation as nested data structure."
    , text "Lambda giver bedre definition af funktion"
    , text "Soundness"
    , text "Completeness"
    , text "Decidability"
    , text "Church-Turing thesis"
    , text "https://mathoverflow.net/questions/247832/the-halting-problem-and-churchs-thesis"
    , text "Gödel"
    , text "https://math.stackexchange.com/questions/1319149/what-breaks-the-turing-completeness-of-simply-typed-lambda-calculus "

    --, Boolean.table [ "S", "F" ] 1
    , header "Boolean"
    , Boolean.cartesianProduct [ "S", "F" ]
    , Boolean.cartesianProduct [ "1", "2", "3" ]
    , Table.table

    --, Table.table
    , header "Intutionistisk"
    , IPL.drawExample
    , header "Introduktion"
    , Intro.intro
    , header "Deduktion"
    , Deduktion.reglerne
    , header "Billede"
    , billede "IPLrules.png"
    , text "https://www.youtube.com/watch?v=iXZR1v3YN-8"
    , text "https://www.youtube.com/watch?v=LkIRsNj9T-8"

    -- Kilder
    , header "Læs mere"
    ]


view model =
    indhold
        |> List.map elementWrap
        |> column
            [ spacing 32
            , padding 64
            , E.centerX
            , width (maximum 800 shrink)
            ]
        |> E.layout []


texBlock tex =
    text <| K.print <| display tex


texin tex =
    K.print <| inline tex


p : String -> Element Msg
p streng =
    E.paragraph [] [ text streng ]


para : List String -> Element Msg
para strenge =
    E.paragraph [] <| List.map text strenge


footnote streng =
    E.paragraph [ Font.size 14 ] <| [ text streng ]


billede file =
    E.image [ maxWidth ] { src = "img/" ++ file, description = "Test" }


maxWidth =
    width (maximum 800 fill)


headerPadding =
    { bottom = 0
    , top = 200
    , right = 0
    , left = -64
    }


header string =
    string |> text |> el [ Font.size 30, paddingEach headerPadding ]


elementWrap element =
    el [] <| element



-- type alias Model =
--     { menuItems : List (Toggleable MenuItem) }
-- type alias MenuItem =
--     { overskrift : String, indhold : List (Element Msg) }
-- items : List (Toggleable MenuItem)
-- items =
--     [ Open <|
--         MenuItem "Boolean"
--             [ Boolean.cartesianProduct [ "S", "F" ]
--             , Boolean.cartesianProduct [ "1", "2", "3" ]
--             , Table.table
--             ]
--     , Closed <|
--         MenuItem "Intutionistisk"
--             [ text "Hej"
--             , IPL.drawExample
--             ]
--     , Closed <|
--         MenuItem "Deduktion"
--             [ text "Nej"
--             , Deduktion.reglerne
--             ]
--     ]
-- initial : Model
-- initial =
--     Model items
-- type Msg
--     = Toggle Int
-- update : Msg -> Model -> Model
-- update msg model =
--     case msg of
--         Toggle index ->
--             { model
--                 | menuItems = List.indexedMap (toggleByIndex index) model.menuItems
--             }
-- toggleByIndex : Int -> Int -> Toggleable a -> Toggleable a
-- toggleByIndex indexToToggle currentIndex toggleable =
--     if indexToToggle == currentIndex then
--         Toggleable.toggle toggleable
--     else
--         toggleable
-- view model =
--     E.layout [] <|
--         column
--             [ spacing 64
--             , padding 64
--             , E.centerX
--             , width (maximum 800 shrink)
--             ]
--         <|
--             List.map elementWrap <|
--                 List.indexedMap viewMenuItem model.menuItems
-- viewMenuItem : Int -> Toggleable MenuItem -> Element Msg
-- viewMenuItem index tg =
--     case tg of
--         Open mi ->
--             openedItem index mi
--         Closed mi ->
--             closedItem index mi
-- openedItem : Int -> MenuItem -> Element Msg
-- openedItem index mi =
--     column [ spacing 32 ] <| [ row [ EEvents.onClick <| Toggle index ] [ el [ pointer ] (text "▼ "), header mi.overskrift ] ] ++ mi.indhold
--row [ EEvents.onClick <| Toggle index ]
--    [ text "▼ ", text mi.overskrift, el [] (text mi.details) ]
-- closedItem : Int -> MenuItem -> Element Msg
-- closedItem index mi =
--     column [ spacing 32 ] [ row [ EEvents.onClick <| Toggle index ] [ el [ pointer ] (text "► "), header mi.overskrift ] ]


main =
    Browser.sandbox
        { init = ()

        --, update = update
        , update = \x y -> y
        , view = view
        }
