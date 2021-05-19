module Hello exposing (..)

import Debug exposing (toString)
import Html exposing (Html, div, p)
import Element exposing (Element, none, el, text, row, column, alignRight, alignLeft, alignTop, alignBottom, fill, fillPortion, width, rgb255, spacing, centerX, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

import Browser
import Html.Attributes as HA
import Markdown.Option exposing (..)
import Markdown.Render exposing (MarkdownMsg(..), MarkdownOutput(..))

--- MODEL ---

type Prop
    = Atom Atomic
    | New PropConnective Prop Prop


type alias NyProp =
    { connective : PropConnective
    , leftArgument : Prop
    , rightArgument : Prop
    }


type alias PropConnective =
    { symbol : String
    , name : String
    }


type Atomic
    = A
    | B
    | C

type Hypothesis
    = U
    | V
    | W

type alias HDerivation =
    { hypothesis: Hypothesis
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
    { connective: PropConnective
    , label: String
    , introduction: DeductionRule
    , elimination: DeductionRule
    }

and =
    PropConnective "∧" "og"

andMeta = 
    { connective = and
    , label = "konjunktion"
    , introduction = andIntro
    , elimination = andElim
    }

andIntro: DeductionRule
andIntro = 
    { premises = [Atom A, Atom B]
    , hDerivations = []
    , conclusions = [New and (Atom A) (Atom B)]
    }

andElim: DeductionRule
andElim = 
    { premises = [New and (Atom A) (Atom B)]
    , hDerivations = []
    , conclusions = [Atom A, Atom B]
    }

or = PropConnective "∨" "eller"

orMeta = 
    { connective = or
    , label = "disjunktion"
    , introduction = orIntro
    , elimination = orElim
    }

orIntro: DeductionRule
orIntro = 
    { premises = [Atom A]
    , hDerivations = []
    , conclusions = [New or (Atom A) (Atom B), New or (Atom B) (Atom A)]
    }

orElim: DeductionRule
orElim = 
    { premises = [New or (Atom A) (Atom B)]
    , hDerivations = 
        [ HDerivation U (Atom A) (Atom C)
        , HDerivation V (Atom A) (Atom C)
        ]
    , conclusions = [Atom C]
    }

implication = PropConnective "→" "medfører"

implicationMeta = 
    { connective = implication
    , label = "implikation"
    , introduction = implicationIntro
    , elimination = implicationElim
    }

implicationIntro: DeductionRule
implicationIntro = 
    { premises = []
    , hDerivations = [HDerivation U (Atom A) (Atom B)]
    , conclusions = [New implication (Atom A) (Atom B)]
    }

implicationElim: DeductionRule
implicationElim = 
    { premises = [Atom A, New implication (Atom A) (Atom B)]
    , hDerivations = []
    , conclusions = [Atom B]
    }
--- VIEW ---
separator = el [width fill, padding 1, Background.color (rgb255 0 0 0) ] none

atomString : Atomic -> String
atomString atom =
    case atom of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

hypothesisString : HDerivation -> String
hypothesisString hyp =
    case hyp.hypothesis of
        U -> "u"
        V -> "v"
        W -> "w"


propString : Prop -> String
propString prop =
            case prop of
                Atom atom ->
                    atomString atom

                New connective left right ->
                    "(" ++ propString left ++ connective.symbol ++ propString right ++ ")"


drawProp : Prop -> Element msg
drawProp prop =
    el [centerX, alignBottom] (propString prop |> Element.text)

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
    column [centerX] 
        (  [row [width fill, spacing 32] 
            ( List.map drawProp rule.premises
            ++List.map drawHDerivation rule.hDerivations
            )]
        ++ [separator]
        ++ [row [width fill, spacing 32] (List.map drawProp rule.conclusions)]
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
    row [width fill, spacing 32]
        [ row [width (fillPortion 1), alignTop, alignLeft] [text meta.label]
        , row [width (fillPortion 2)]
            [column [spacing 16, centerX]
                [ text "introduktion"
                , drawDeductionRule meta.introduction
                ]
            ]
        , row [width (fillPortion 2)]
            [column [spacing 16, centerX]
                [ text "elimination"
                , drawDeductionRule meta.elimination
                ]
            ]
        ]


mainDraw = 
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
        , spacing 32
        ]
        [ drawMeta implicationMeta
        , drawMeta andMeta
        , drawMeta orMeta
        ]
{-        [ drawProp x
        , drawProp y
        , drawProp z
        , drawDeductionRule andIntro
        , drawDeductionRule andElim
        , drawDeductionRules [andIntro, andElim]
        ]-}
{-
main : Html msg
main =
    Element.layout [] mainDraw
-}




--- MARKDOWN VIEW ---
main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


type alias Model =
    { sourceText : String
    }


type Msg
    = NoOp
    | MarkdownMsg Markdown.Render.MarkdownMsg


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { sourceText = svgText }, Cmd.none )


svgText =
    """
# This is a test
one
@removed[two (x)]
@added[two (y)]
three
Some math:
$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$
@highlight[An svg figure]:
@@svg
<svg width="300" height="100">
  <circle cx="250" cy="50" r="40" stroke="blue" stroke-width="3" fill="cyan" />
</svg>
@red[Some Html entities]:
&forall; (&bbA;:&caU;): &bbA; &to; &bbB;
@blue[API]
```elm
Markdown.Render.toHtml ExtendedMath model.sourceText
  |> Html.map MarkdownMsg
```
"""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MarkdownMsg _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ HA.style "margin" "50px", HA.style "width" "500px" ]
        [ Markdown.Render.toHtml ExtendedMath model.sourceText
            |> Html.map MarkdownMsg
        ]