module Main exposing (main)

import Boolean
import Browser
import Deduktion
import Element as E exposing (Element, column, el, fill, height, html, maximum, padding, paddingEach, paragraph, pointer, rgb, rotate, row, shrink, spacing, text, width)
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
    , header "Trivialitet og tautologi"
    , paragraph []
        [ text "Vi vil i dag få et overblik over propositionel logik, både klassisk og konstruktivistisk, og som sidegevinst lære om funktionel, typesikker programmering."
        ]
    , p "Hvad er implikation?"
    , p "Fra sande antagelser kan man ikke få falske konklusioner."
    , p "Vi vil se på fire forskellige former for implikation:"
    , p "Nødvendighed (hvis P er sand så er Q også sand):"
    , texBlock "P \\models Q"
    , p "Bevisbarhed (P beviser Q):"
    , texBlock "P \\vdash Q"
    , p "Implikationsrelationen:"
    , texBlock "\\Rightarrow: \\mathbb{B}^2 \\rightarrow \\mathbb{B}"
    , p "Implikationsudsagnet:"
    , texBlock "P \\supset Q"
    , p "Intutionistisk logik skrotter nødvendighed og implikationsrelationen."
    , footnote "Hvorfor er implikation sandt for falske antagelser? Ikke åbenlyst fra sandhedstabel-definitionen. Falsk er umuligt at introducere i ND, men man kan eliminere den til alt (Da den var umulig at introducere er det ligemeget )"

    -- Curry-Howard-Lambek
    , text ""
    , para [ "Matematik", texin " \\sim ", "Logik", texin " \\sim ", "Programmering" ]
    , header
        "Curry-Howard-Lambek correspondence"

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
        \\quad|\\quad \\omega \\supset \\theta 
        \\quad|\\quad \\neg \\theta 
        """
    , para
        [ "Vi definerer mængden "
        , texin "\\mathbb{U}"
        , " som den mindste mængde der indeholder alle udtryk på ovenstående form."
        ]
    , footnote "Noget binder implicit stærkere end andet; man kan visualisere det som et træ. Vi springer over: motivation, frie variable, variabel substitution, mm. Rekursivt == Strukturel induktion"

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
    , column []
        [ footnote "Bemærk at:"
        , footnote "- Konjunktion og disjunktion svarer til hhv. gange og plus eller max og min. "
        , footnote "- Vi kunne have nøjes med blot én binær operator, NOR eller NAND."
        , footnote "- Alle trinære operatorer kan sammensættes af binære operatorer"
        ]
    , para
        [ "I klassisk semantik kan implikation anses som et sammensat udsagn (de har samme sandhedstabel): "
        , texin "\\quad P \\Rightarrow Q \\quad \\equiv \\quad \\neg P \\vee Q"
        ]
    , para
        [ "Vi vil se om lidt at dette er en tautologi:"
        , texin "\\quad P \\Rightarrow Q \\quad \\Leftrightarrow \\quad \\neg P \\vee Q \\quad"
        , "Intutionistisk gælder kun (for alle P,Q): "
        , texin "\\quad P \\Rightarrow Q \\quad \\Leftrightarrow \\quad \\neg P \\vee Q \\quad"
        ]
    , para
        [ "(Modstridsbevis) Klassiske tautologier:"
        , texin "\\quad \\neg P \\Leftrightarrow (P \\Rightarrow 0) \\quad"
        , "og"
        , texin "\\quad \\neg \\neg P \\Leftrightarrow [(P \\Rightarrow 0) \\Rightarrow 0] \\quad"
        , "og"
        , texin "\\quad P \\Leftrightarrow [(P \\Rightarrow 0) \\Rightarrow 0] \\quad"
        ]
    , footnote "Kun de to første gælder intutionistisk pr notationel definition af negation."
    , footnote """In intuitionistic logic, de Morgan's law often refers to the one of de Morgan's four laws that is not an intuitionistic tautology, namely ¬(P∧Q)→(¬P∨¬Q) for any P,Q. ... De Morgan's law is equivalent to weak excluded middle."""
    , footnote "https://ncatlab.org/nlab/show/weak+excluded+middle"
    , footnote "In logic, the principle of weak excluded middle says that for any proposition P we have ¬P∨¬¬P."

    -- Bool table
    , para [ "Alle funktioner ", texin "\\sim:\\mathbb{B}^2 \\rightarrow \\mathbb{B}" ]
    , Table.table
    , footnote "Vis modus ponens og kontroponerede og mere som sandhedstabeller"
    , text ""
    , para
        [ "En fortolkning (interpretation/valuation/assignment) "
        , texin "\\rho"
        , " er enhver funktion "
        , texin "\\rho : \\mathbb{V} \\rightarrow \\mathbb{B} "
        ]
    , para
        [ "Semantikken "
        , texin "\\llbracket \\omega \\rrbracket_\\rho : \\mathbb{U} \\rightarrow \\mathbb{B}"
        , " af et udtryk  "
        , texin "\\omega"
        , " under fortolkningen "
        , texin "\\rho"
        , " er defineret rekursivt som:"
        ]
    , texBlock "\\llbracket \\omega \\rrbracket_\\rho = \\rho(\\omega)"
    , texBlock "\\llbracket \\omega \\wedge \\theta \\rrbracket_\\rho = \\llbracket \\omega \\rrbracket_\\rho \\wedge \\llbracket \\theta \\rrbracket_\\rho"
    , texBlock "\\llbracket \\omega \\vee \\theta \\rrbracket_\\rho = \\llbracket \\omega \\rrbracket_\\rho \\vee \\llbracket \\theta \\rrbracket_\\rho"
    , texBlock "\\llbracket \\omega \\supset \\theta \\rrbracket_\\rho = \\llbracket \\omega \\rrbracket_\\rho \\Rightarrow \\llbracket \\theta \\rrbracket_\\rho"
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
        [ "En fortolkning "
        , texin "\\rho"
        , " modellerer/opfylder et udtryk "
        , texin "\\omega"
        , " hviss "
        , texin "\\omega"
        , " er sandt under "
        , texin "\\rho"

        --, texin "\\llbracket \\omega \\rrbracket_\\rho = 1"
        , " og vi skriver i så fald "
        , texin "\\rho \\models \\omega"
        ]
    , para
        [ "En mængde af udtryk "
        , texin "\\Gamma"
        , " nødvendiggør (entails) "
        , texin "\\omega"
        , " hviss for alle "
        , texin "\\theta \\in \\Gamma"
        , " og for alle fortolkninger "
        , texin "\\rho"
        , ": Hvis "
        , texin "\\rho \\models \\theta"
        , ", så "
        , texin "\\rho \\models \\omega"
        , ". Vi skriver i så fald "
        , texin "\\quad \\Gamma \\models \\omega"
        ]
    , para
        [ "Udtrykket "
        , texin "\\omega"
        , " er gyldigt (valid) hviss det er sandt under alle fortolkninger. Vi skriver i så fald "
        , texin "\\quad \\models \\omega \\quad"
        , "Vi siger også at "
        , texin "\\omega"
        , " er en tautologi."
        ]
    , p "I klassisk logik afgør vi altså om et udsagn er en tautologi ved at kigge på sandhedstabellen."
    , footnote "Semantikken retfærdiggør Modus Ponens."
    , text "Deduktionssætningen:"
    , p ""
    , footnote "Elm automatisk typechecking af editor -> decidable fordi intutionistisk"
    , footnote "Sandhedstabeller og andre semantikker kan bruges til at søge efter beviser (eller sige om det er sandt eller falsk i alle modeller), men giver ikke god forklaring (konstruktivt bevis). Semantik mindre interessant end deduktion, intutionistisk bedre end klassisk ND. "
    , footnote "Skal ikke argumentere for klassisk completeness and soundness. Mange ND-systemer opfylder det, vi definerer en klassisk ND til at overholde dette. Mangler at overbevise om eksistens ved at vise specifikt vidne. Bevis per strukturel induktion på størelsen af beviset. Vil hellere antage eksistens og vise hvad der ikke gælder intutionistisk ift hvad der gælder klassisk semantisk."
    , footnote "Eksistens nemt? BAre tag nogle tautologier som aksiomer og så modus ponens som inferensregel."
    , footnote "Lav eksempelbevis: dobbeltnegation hvis og kun hvis LEM."
    , footnote "Vi ønsker i prioriteret rækkefølge: soundness, completeness, decidability."
    , text "Fuldstændighed og sundhed"
    , header "Beviselighed"
    , text "Hvad er et bevis?"
    , header "Naturlig Deduktion"

    -- Konstruktiv logik
    , text ""
    , header "Konstruktiv logik"
    , footnote "Intutionistisk udsangslogik logik er decidable <=> typechecking er trivielt"
    , p "I konstruktiv logik har udsagn ikke sandhedsværdier. Vi taler i stedet om vurderinger (judgments) vi laver om objekter. Intutionistisk udsagnslogik handler om hvornår vi kan vurdere et udsagn til at være sandt."
    , texBlock "\\underbrace{ \\overbrace{\\omega}^{\\text{objekt}} \\text{sand} }_{\\text{vurdering}}"
    , billede "Pfenning-judgment-evidence.png"

    -- Hypotetisk udledning
    , header "Hypotetisk udledning"
    , billede "Pfenning-judgment-hypothetical.png"
    , billede "Pfenning-implication.png"
    , billede "Pfenning-implication-2.png"
    , billede "Pfenning-disjunction-falsehood.png"
    , billede "Pfenning-negation.png"

    -- Rules of inference
    , header "Inferensregler"
    , Deduktion.reglerne
    , billede "Pfenning-table.png"
    , header ""

    -- Simply Typed Lambda Calculus
    , header "Vurderinger i typed lambda calculus"
    , billede "ms-STLC.png"
    , billede "ms-Gentzen.png"
    , billede "ms-Gentzen-sideways.png"
    , billede "ms-Gentzen-sideways-change.png"
    , billede "ms-Gentzen-sideways-change-add.png"

    --, header "Rice' Sætning"
    --, header "Church-Rosser Sætning"
    --, header ""
    --, header "Halting Problem"
    --, header "Decidability"
    --, header "Semantik"
    --, header "Naturlig deduktion"
    --, header "Historie"
    --, header "Klassisk"
    --, text "https://www.youtube.com/watch?v=HMoPqR4-jDg&t=68s&ab_channel=KyleBanick"
    --, text "WFF -> Propositions, "
    , text """
The following can be seen as a form of completeness of the ¸-calculus.
1.5.20. Theorem (Kleene). All recursive functions are ¸-de¯nable.
Proof. By the above lemmas.
                """

    --, text "nemt nok at vise at nor/nand er nok"
    --, text "https://www.youtube.com/watch?v=xZY35QQf9kA&list=PLjJhPCaCziSRSUtQiTA_yx5TJ76G_EqUJ&index=6&ab_channel=AntonioMontalban"
    --, text "Binary connectives are enough"
    , text "k-ary connectives in intutionistic logic has nice interpretation as nested data structure."
    , text "Lambda giver bedre definition af funktion"
    , text "Soundness"
    , text "Completeness"

    --, text "Decidability"
    , text "Church-Turing thesis"
    , text "https://mathoverflow.net/questions/247832/the-halting-problem-and-churchs-thesis"
    , text "Gödel"
    , text "https://math.stackexchange.com/questions/1319149/what-breaks-the-turing-completeness-of-simply-typed-lambda-calculus "

    --, Boolean.table [ "S", "F" ] 1
    --, header "Boolean"
    --, Boolean.cartesianProduct [ "S", "F" ]
    --, Boolean.cartesianProduct [ "1", "2", "3" ]
    --, Table.table
    , Table.table
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
    , text "Category theory"
    , text "Agda"
    , text "Klassisk logik og ZFC"
    , text "Type theory foundations"
    , text "ELm introduction playlist + github med mange"
    , text "Historieforedraget"
    , text "Pfenning 2000 lecture notes"
    , text "Lambda calculus book"
    , text "Bøger: T&PL, S&I, Homotopy, ..."

    -- lecture notes
    , text ""

    -- Bevisteori
    , text ""
    , header "Bevisteori"

    --, billede "remarkable/210521 Proof/210521 Proof - page 1.png"
    , billede "remarkable/210521 Proof/210521 Proof - page 2.png"
    , header "Konjunktion"
    , billede "remarkable/210521 Proof/210521 Proof - page 3.png"
    , billede "remarkable/210521 Proof/210521 Proof - page 4.png"
    , header "Implikation"
    , billede "remarkable/210521 Proof/210521 Proof - page 5.png"
    , billede "remarkable/210521 Proof/210521 Proof - page 6.png"
    , header "Disjunktion"
    , billede "remarkable/210521 Proof/210521 Proof - page 7.png"
    , billede "remarkable/210521 Proof/210521 Proof - page 8.png"
    , header "Absurditet"
    , billede "remarkable/210521 Proof/210521 Proof - page 9.png"
    , header "Negation"
    , billede "remarkable/210521 Proof/210521 Proof - page 10.png"
    , billede "remarkable/210521 Proof/210521 Proof - page 11.png"

    -- Typer
    , text ""
    , header "Type teori"

    --, billede "remarkable/210521 Type/210521 Type - page 1.png"
    , billede "remarkable/210521 Type/210521 Type - page 2.png"
    , header "Refleksiv og transitiv"
    , E.el [ rotate (turns -(1 / 4)) ] <| billede "remarkable/210521 Type/210521 Type - page 3.png"
    , header "Konsekvens"
    , billede "remarkable/210521 Type/210521 Type - page 4.png"
    , text ""
    , header "Konjunktion"
    , E.el [ rotate (turns -(1 / 4)) ] <| billede "remarkable/210521 Type/210521 Type - page 5.png"

    --, billede "remarkable/210521 Type/210521 Type - page 6.png"
    --, billede "remarkable/210521 Type/210521 Type - page 7.png"
    , header "Ekvivalens"
    , billede "remarkable/210521 Type/210521 Type - page 8.png"

    --, billede "remarkable/210521 Type/210521 Type - page 9.png"
    , billede "remarkable/210521 Type/210521 Type - page 10.png"
    , header "Heyting og Boolean algebra"
    , billede "remarkable/210521 Type/210521 Type - page 11.png"
    , billede "remarkable/210521 Type/210521 Type - page 12.png"

    --, billede "remarkable/210521 Type/210521 Type - page 13.png"
    , billede "remarkable/210521 Type/210521 Type - page 14.png"
    , billede "remarkable/210521 Type/210521 Type - page 15.png"
    , billede "remarkable/210521 Type/210521 Type - page 16.png"
    , text ""
    , header "Curry-Howard-Lambek"

    --, billede "remarkable/210512 Curry-Howard - page 1.png"
    --, billede "remarkable/210512 Curry-Howard - page 2.png"
    --, billede "remarkable/210512 Curry-Howard - page 3.png"
    --, billede "remarkable/210512 Curry-Howard - page 4.png"
    --, billede "remarkable/210512 Curry-Howard - page 5.png"
    --, billede "remarkable/210512 Curry-Howard - page 6.png"
    --, billede "remarkable/210512 Curry-Howard - page 7.png"
    --, billede "remarkable/210512 Curry-Howard - page 8.png"
    --, billede "remarkable/210512 Curry-Howard - page 9.png"
    --, billede "remarkable/210512 Curry-Howard - page 10.png"
    --, billede "remarkable/210512 Curry-Howard - page 11.png"
    --, billede "remarkable/210512 Curry-Howard - page 12.png"
    --, billede "remarkable/210512 Curry-Howard - page 13.png"
    , text ""
    , header "Kategori teori"
    , p "En kategori er en samling af objekter med morfismer. Alle objekter har en id-morfisme og morfismer komponerer associativt."

    --, billede "remarkable/210520 Category theory - page 1.png"
    --, billede "remarkable/210520 Category theory - page 2.png"
    , billede "remarkable/210520 Category theory - page 3.png"
    , billede "remarkable/210520 Category theory - page 4.png"

    --, billede "remarkable/210520 Category theory - page 5.png"
    , header "Relation, funktion, invers"
    , billede "remarkable/210520 Category theory - page 6.png"
    , billede "remarkable/210520 Category theory - page 7.png"
    , billede "remarkable/210520 Category theory - page 8.png"
    , header "Epi- og monomorfisme"
    , billede "remarkable/210520 Category theory - page 9.png"

    --, billede "remarkable/210520 Category theory - page 10.png"
    --, billede "remarkable/210520 Category theory - page 11.png"
    --, billede "remarkable/210520 Category theory - page 12.png"
    , header "Preorden"
    , billede "remarkable/210520 Category theory - page 13.png"
    , billede "remarkable/210520 Category theory - page 14.png"
    , billede "remarkable/210520 Category theory - page 15.png"

    --, billede "remarkable/210520 Category theory - page 16.png"
    --, billede "remarkable/210520 Category theory - page 17.png"
    --, billede "remarkable/210520 Category theory - page 18.png"
    --, billede "remarkable/210520 Category theory - page 19.png"
    --, billede "remarkable/210520 Category theory - page 20.png"
    , billede "remarkable/210520 Category theory - page 21.png"
    , billede "remarkable/210520 Category theory - page 22.png"
    , billede "remarkable/210520 Category theory - page 23.png"
    , billede "remarkable/210520 Category theory - page 24.png"
    , text ""
    , header "Den omvendte kategori"
    , billede "remarkable/210520 Category theory - page 25.png"
    , text ""
    , header "Cartesisk produkt"
    , billede "remarkable/210520 Category theory - page 26.png"
    , billede "remarkable/210520 Category theory - page 27.png"
    , text ""
    , header "Coprodukt eller sum-typen"
    , billede "remarkable/210520 Category theory - page 28.png"
    , billede "remarkable/210520 Category theory - page 29.png"
    , text ""
    , header "Algebraiske datatyper"
    , billede "remarkable/210520 Category theory - page 30.png"
    , billede "remarkable/210520 Category theory - page 31.png"
    , billede "remarkable/210520 Category theory - page 32.png"
    , billede "remarkable/210520 Category theory - page 33.png"

    --, billede "remarkable/210520 Category theory - page 34.png"
    --, billede "remarkable/210520 Category theory - page 35.png"
    --, billede "remarkable/210520 Category theory - page 36.png"
    --, billede "remarkable/210520 Category theory - page 37.png"
    --, billede "remarkable/210520 Category theory - page 38.png"
    --, billede "remarkable/210520 Category theory - page 39.png"
    --, billede "remarkable/210520 Category theory - page 40.png"
    --, billede "remarkable/210520 Category theory - page 41.png"
    --, billede "remarkable/210520 Category theory - page 42.png"
    --, billede "remarkable/210520 Category theory - page 43.png"
    --, billede "remarkable/210520 Category theory - page 44.png"
    --, billede "remarkable/210520 Category theory - page 45.png"
    -- Kategori teori
    , text ""
    , header "Kategori teori | Funktionstypen, exponentialobjektet"
    , billede "remarkable/210521 Category/210521 Category - page 1.png"
    , billede "remarkable/210521 Category/210521 Category - page 2.png"
    , billede "remarkable/210521 Category/210521 Category - page 3.png"
    , billede "remarkable/210521 Category/210521 Category - page 4.png"
    , billede "remarkable/210521 Category/210521 Category - page 5.png"
    , billede "remarkable/210521 Category/210521 Category - page 6.png"
    , billede "remarkable/210521 Category/210521 Category - page 7.png"
    , billede "remarkable/210521 Category/210521 Category - page 8.png"
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


bcut file =
    E.image


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
