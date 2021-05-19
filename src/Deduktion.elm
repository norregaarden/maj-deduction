module Deduktion exposing (..)

import Browser
import Debug exposing (toString)
import Element exposing (Element, paddingEach, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, explain, fill, fillPortion, height, none, padding, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html, div, p)
import Html.Attributes as HA
import Markdown.Option exposing (..)
import Markdown.Render exposing (MarkdownMsg(..), MarkdownOutput(..))



--- MODEL ---


type Prop
    = Atom Atomic
    | New PropConnective Prop Prop


type alias PropConnective =
    { symbol : String
    , name : String
    }


type Atomic
    = A
    | B
    | C
    | E


type Hypothesis
    = U
    | V
    | W


type alias HDerivation =
    { hypothesis : Hypothesis
    , assumption : Prop
    , conclusion : Prop
    }


type alias DeductionRule =
    { premises : List Prop
    , hDerivations : List HDerivation
    , conclusions : List Prop
    }



--- CONSTANTS ---


type alias RuleMeta =
    { connective : PropConnective
    , label : String
    , introduction : DeductionRule
    , elimination : DeductionRule
    }


and =
    PropConnective "∧" "og"


andMeta =
    { connective = and
    , label = "konjunktion"
    , introduction = andIntro
    , elimination = andElim
    }


andIntro : DeductionRule
andIntro =
    { premises = [ Atom A, Atom B ]
    , hDerivations = []
    , conclusions = [ New and (Atom A) (Atom B) ]
    }


andElim : DeductionRule
andElim =
    { premises = [ New and (Atom A) (Atom B) ]
    , hDerivations = []
    , conclusions = [ Atom A, Atom B ]
    }


or =
    PropConnective "∨" "eller"


orMeta =
    { connective = or
    , label = "disjunktion"
    , introduction = orIntro
    , elimination = orElim
    }


orIntro : DeductionRule
orIntro =
    { premises = [ Atom A ]
    , hDerivations = []
    , conclusions = [ New or (Atom A) (Atom B), New or (Atom B) (Atom A) ]
    }


orElim : DeductionRule
orElim =
    { premises = [ New or (Atom A) (Atom B) ]
    , hDerivations =
        [ HDerivation U (Atom A) (Atom C)
        , HDerivation V (Atom A) (Atom C)
        ]
    , conclusions = [ Atom C ]
    }


implication =
    PropConnective "→" "medfører"


implicationMeta =
    { connective = implication
    , label = "implikation"
    , introduction = implicationIntro
    , elimination = implicationElim
    }


implicationIntro : DeductionRule
implicationIntro =
    { premises = []
    , hDerivations = [ HDerivation U (Atom A) (Atom B) ]
    , conclusions = [ New implication (Atom A) (Atom B) ]
    }


implicationElim : DeductionRule
implicationElim =
    { premises = [ Atom A, New implication (Atom A) (Atom B) ]
    , hDerivations = []
    , conclusions = [ Atom B ]
    }



--- VIEW ---


separator =
    el [ width fill, padding 1, Background.color (rgb 0 0 0) ] none


atomString : Atomic -> String
atomString atom =
    case atom of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        E ->
            "erorr"


hypothesisString : HDerivation -> String
hypothesisString hyp =
    case hyp.hypothesis of
        U ->
            "u"

        V ->
            "v"

        W ->
            "w"


propString : Prop -> String
propString prop =
    case prop of
        Atom atom ->
            atomString atom

        New connective left right ->
            "(" ++ propString left ++ connective.symbol ++ propString right ++ ")"


drawProp : Prop -> Element msg
drawProp prop =
    el [ centerX, alignBottom ] (propString prop |> Element.text)


drawHDerivation : HDerivation -> Element msg
drawHDerivation hder =
    column []
        [ text <| hypothesisString hder
        , separator
        , text <| propString hder.assumption
        , text <| propString hder.conclusion
        ]


drawDeductionRule : DeductionRule -> Element msg
drawDeductionRule rule =
    column [ centerX, alignBottom ]
        ([ row [ width fill, spacing 32 ]
            (List.map drawProp rule.premises
                ++ List.map drawHDerivation rule.hDerivations
            )
         ]
            ++ [ separator ]
            ++ [ row [ width fill, height fill, spacing 32 ] (List.map drawProp rule.conclusions) ]
        )


drawDeductionRules : List DeductionRule -> Element msg
drawDeductionRules ruleList =
    row
        [ centerX
        , centerY
        , spacing 32
        ]
        (List.map (\rule -> drawDeductionRule rule) ruleList)


drawMeta : RuleMeta -> Element msg
drawMeta meta =
    row [ width fill, spacing 64 ]
        [ row [ width (fillPortion 1), alignTop, alignLeft ]
            [ el [ Font.bold ] <| text meta.label
            ]
        , row [ width (fillPortion 2), alignTop, height fill ]
            [ column [ spacing 32, centerX, height fill, width fill ]
                [ el [ centerX ] <| text "introduktion"
                , drawDeductionRule meta.introduction
                ]
            ]
        , row [ width (fillPortion 2), alignTop, height fill ]
            [ column [ spacing 32, centerX, height fill, width fill ]
                [ el [ centerX ] <| text "elimination"
                , drawDeductionRule meta.elimination
                ]
            ]
        ]


reglerne =
    let
        x =
            New and (Atom A) (Atom B)

        y =
            Atom A

        z =
            New and x y
    in
    column
        [ centerX
        , centerY
        , spacing 64
        ]
        [ text "hej"
        , drawMeta implicationMeta
        , drawMeta andMeta
        , drawMeta orMeta

        --derivation [ Atom A, Atom B ] [ andIntroApply, andIntroApply ]
        ]



{- type alias RuleApplication =
       { rule : DeductionRule
       , lines : List Int
       , premises : Prop
       }


   andIntroApply : RuleApplication
   andIntroApply =
       { rule = andIntro
       , premi
       }
-}


type alias RuleApplication =
    Maybe Prop -> Maybe Prop -> Maybe HDerivation -> Maybe HDerivation -> Prop


andIntroApply : RuleApplication
andIntroApply p1 p2 hd1 hd2 =
    case p1 of
        Just pp ->
            case p2 of
                Just qq ->
                    New and pp qq

                Nothing ->
                    Atom E

        Nothing ->
            Atom E


derivation : List Prop -> List RuleApplication -> Element msg
derivation premises rules =
    column [] <|
        List.map (\p -> drawProp p) premises



-- ++ List.map () rules
{- [ drawProp x
   , drawProp y
   , drawProp z
   , drawDeductionRule andIntro
   , drawDeductionRule andElim
   , drawDeductionRules [andIntro, andElim]
   ]
-}
