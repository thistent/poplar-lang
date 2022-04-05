module Main exposing (..)

-- import Html.Attributes exposing (id, style)

import Browser
import Color
import Element as El exposing (Attribute, Element, el)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Math as Math exposing (MathExpr(..))
import Html exposing (Html)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Filters.Attributes exposing (..)
import TypedSvg.Types as Types exposing (EdgeMode(..), FontWeight(..), Paint(..))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subs
        , update = update
        , view = view
        }



-- Model --


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init () =
    ( 0, Cmd.none )


type Msg
    = NoOp


type alias ColorScheme =
    { fg : El.Color
    , bg : El.Color
    , link : El.Color
    , math : Math.ColorScheme
    }



-- Subs --


subs : Model -> Sub Msg
subs model =
    Sub.batch []



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- View --


f : Float
f =
    42


m : Float
m =
    f * 0.5


view : Model -> Html Msg
view model =
    El.layout
        [ El.width El.fill
        , El.height El.fill
        , Font.color newspaper.fg --<| El.rgb 0.8 0.8 0.8

        --, Font.family [ Font.serif ]
        , getExtFont "Dosis"

        --, getExtFont "JetBrains"
        --, getExtFont "Roboto"
        , Font.size 18
        , Bg.color newspaper.bg --<| El.rgb 0 0 0
        ]
    <|
        El.row
            [ El.width El.fill
            , El.height El.fill
            ]
            [ El.column
                [ El.alignTop
                , El.height El.fill
                , newspaper.fg |> Math.changeAlpha 0.1 |> Bg.color
                , Border.shadow
                    { offset = ( 2.0, 0 )
                    , size = 4.0
                    , blur = 10
                    , color = newspaper.fg |> Math.changeAlpha 0.2
                    }
                , El.padding <| round f
                , El.spacing <| round <| f * 0.25
                ]
                [ El.image [ El.width El.fill, El.centerX ]
                    { src = "assets/poplar.svg"
                    , description = "Poplar Lang"
                    }
                , el [ Font.size 16, El.centerX ] <|
                    El.text "Simplifying Plutus Programming"
                ]
            , El.textColumn
                [ El.width El.fill
                , El.height El.fill
                , El.padding <| round f
                , El.spacing <| round f
                ]
                [ El.column []
                    [ el [ El.width El.fill ] <|
                        el
                            [ El.centerY
                            , Font.bold
                            , Font.size <| round f
                            ]
                        <|
                            El.text "Hello, World! "
                    , El.paragraph []
                        [ El.text "Welcome to the "
                        , el [ Font.bold ] <| El.text "Poplar"
                        , El.text " page!"
                        ]
                    ]
                , El.paragraph [] <|
                    [ El.text "This page is currently being edited manually,"
                    , El.text " but it will eventually be generated automatically with my"
                    , el [ Font.bold ] <| El.text " distributed idea mapping system "
                    , Math.render 18.0 newspaper.math <|
                        Pars <|
                            Op "DIMS"
                    , El.text "."
                    , El.text " For now, I'm just going to post some of the type theory"
                    , El.text " of Plutus Core in mathematical form,"
                    , El.text " as well as some of the other languages I want to include"
                    , El.text " in my semantic comparative analysis..."
                    ]
                , el [ El.alignLeft, El.centerY ] <|
                    Math.render 18.0 newspaper.math <|
                        ReductionRule "tyvar"
                            (ExprList
                                [ Pars
                                    (OfKind (KVar "K")
                                        (Var Math.alpha)
                                    )
                                , Op Math.isIn
                                , Op Math.context
                                ]
                            )
                            (ExprList
                                [ Op Math.context
                                , Op Math.implies
                                , OfKind (KVar "K")
                                    (Var Math.alpha)
                                ]
                            )
                , el [ El.alignLeft, El.centerY ] <|
                    Math.render 18.0 newspaper.math <|
                        ReductionRule "tyall"
                            (ExprList
                                [ Contains (Op Math.context)
                                    (OfKind (KVar "K") (Var Math.alpha))
                                , Op Math.implies
                                , OfKind (Op Math.star) (TyVar "A")
                                ]
                            )
                            (ExprList
                                [ Op Math.context
                                , Op Math.implies
                                , OfKind (Op Math.star)
                                    (Pars
                                        (ExprList
                                            [ Op "all"
                                            , Var Math.alpha
                                            , KVar "K"
                                            , TyVar "A"
                                            ]
                                        )
                                    )
                                ]
                            )
                , el [ El.alignLeft, El.centerY ] <|
                    Math.render 18.0 newspaper.math <|
                        ReductionRule "tyfix"
                            (SpacedExprs
                                [ ExprList
                                    [ Op Math.context
                                    , Op Math.implies
                                    , TyVar "B" |> OfKind (KVar "K")
                                    ]
                                , ExprList
                                    [ Op Math.context
                                    , Op Math.implies
                                    , TyVar "A"
                                        |> OfKind
                                            (Pars
                                                (ExprList
                                                    [ Pars
                                                        (ExprList
                                                            [ KVar "K"
                                                            , Op Math.arrow
                                                            , Op Math.star
                                                            ]
                                                        )
                                                    , Op Math.arrow
                                                    , Pars
                                                        (ExprList
                                                            [ KVar "K"
                                                            , Op Math.arrow
                                                            , Op Math.star
                                                            ]
                                                        )
                                                    ]
                                                )
                                            )
                                    ]
                                ]
                            )
                            (ExprList
                                [ Op Math.context
                                , Op Math.implies
                                , Pars
                                    (ExprList
                                        [ Op "ifix"
                                        , TyVar "A"
                                        , TyVar "B"
                                        ]
                                    )
                                    |> OfKind (Op Math.star)
                                ]
                            )
                , el [ El.alignLeft, El.centerY ] <|
                    Math.render 18.0 newspaper.math <|
                        ReductionRule "tyfun"
                            (SpacedExprs
                                [ ExprList
                                    [ Op Math.context
                                    , Op Math.implies
                                    , TyVar "A" |> OfKind (Op Math.star)
                                    ]
                                , ExprList
                                    [ Op Math.context
                                    , Op Math.implies
                                    , TyVar "B" |> OfKind (Op Math.star)
                                    ]
                                ]
                            )
                            (ExprList
                                [ Op Math.context
                                , Op Math.implies
                                , Pars
                                    (ExprList
                                        [ TyVar "A"
                                        , Op Math.arrow
                                        , TyVar "B"
                                        ]
                                    )
                                    |> OfKind (Op Math.star)
                                ]
                            )
                , el [ El.alignLeft, El.centerY ] <|
                    Math.render 18.0 newspaper.math <|
                        ReductionRule "tylam"
                            (ExprList
                                [ Contains (Op Math.context)
                                    (OfKind (KVar "J") (Var Math.alpha))
                                , Op Math.implies
                                , OfKind (Op "K") (TyVar "A")
                                ]
                            )
                            (ExprList
                                [ Op Math.context
                                , Op Math.implies
                                , Pars
                                    (ExprList
                                        [ Op "lam"
                                        , Var Math.alpha
                                        , KVar "J"
                                        , TyVar "A"
                                        ]
                                    )
                                    |> OfKind
                                        (Pars
                                            (ExprList
                                                [ KVar "J"
                                                , Op Math.arrow
                                                , KVar "K"
                                                ]
                                            )
                                        )
                                ]
                            )
                , el [ El.alignLeft, El.centerY ] <|
                    Math.render 18.0 newspaper.math <|
                        ReductionRule "tyapp"
                            (SpacedExprs
                                [ ExprList
                                    [ Op Math.context
                                    , Op Math.implies
                                    , OfKind
                                        (Pars
                                            (ExprList
                                                [ KVar "J"
                                                , Op Math.arrow
                                                , KVar "K"
                                                ]
                                            )
                                        )
                                        (TyVar "A")
                                    ]
                                , ExprList
                                    [ Op Math.context
                                    , Op Math.implies
                                    , TyVar "B" |> OfKind (KVar "J")
                                    ]
                                ]
                            )
                            (ExprList
                                [ Op Math.context
                                , Op Math.implies
                                , OfKind (KVar "K") (Apply (TyVar "A") (TyVar "B"))
                                ]
                            )
                ]
            ]



-- Color Schemes --


black : El.Color
black =
    El.rgb 0 0 0


white : El.Color
white =
    El.rgb 1 1 1


newspaper : ColorScheme
newspaper =
    { fg = El.rgb 0.2 0.2 0.2 --0.2 0.15 0
    , bg = El.rgb 0.8 0.8 0.8 --0.88 0.86 0.84
    , link = El.rgb 0 0.3 0.6 -- 0.7 0.1 0
    , math =
        { const = black
        , var = El.rgb 0 0.3 0.9
        , tyVar = El.rgb 0 0.6 0.9
        , kVar = El.rgb 0 0.6 0.3
        , op = El.rgb 0.4 0.4 0.4
        , exp = black
        , frac = El.rgb 0.6 0.6 0.6
        , ofType = black
        , ofKind = black
        , pars = El.rgb 0.3 0.3 0.3
        , lam = El.rgb 0.8 0.3 0
        , reductionRule = black
        , replace = black
        }
    }



-- Other --


getExtFont : String -> Attribute Msg
getExtFont extFont =
    Font.family
        [ Font.external
            { name = extFont
            , url = "https://fonts.googleapis.com/css?family=" ++ extFont
            }
        ]


edges =
    { top = 0, bottom = 0, left = 0, right = 0 }


corners =
    { topLeft = 0, bottomLeft = 0, bottomRight = 0, topRight = 0 }


latex : Float -> Element Msg
latex size =
    el
        [ El.inFront <|
            El.row
                [ getExtFont "Merriweather"
                , El.width El.shrink
                , El.height El.shrink
                , El.moveUp <| size * 0.8
                , El.moveRight <| size * 0.2
                ]
                [ el [] <| El.text " L"
                , el
                    [ El.moveUp <| size * 0.2
                    , El.moveLeft <| size * 0.35
                    , Font.size <| round <| size * 0.85
                    ]
                  <|
                    El.text " A"
                , el [ El.moveLeft <| size * 0.45 ] <| El.text " T"
                , el
                    [ El.moveLeft <| size * 0.65
                    , El.moveDown <| size * 0.25
                    ]
                  <|
                    El.text " E"
                , el
                    [ El.moveLeft <| size * 0.8
                    , El.moveDown <| size * 0.1
                    ]
                  <|
                    El.text " X"
                ]
        , El.width <| El.px <| round <| size * 2.8
        ]
        El.none



-- Not Yet Needed --
{--
svgImage : Element msg
svgImage =
    El.html <|
        svg
            [ viewBox 0 0 600 200
            ]
            [ rect
                [ x <| px 0
                , y <| px 0
                , width <| px 400
                , height <| px 400
                , fill <| Paint <| Color.rgba 0.0 0.0 0.5 1
                ]
                []
            , text_
                [ x (px 75)
                , y (px 75)
                , width <| px 350
                , height <| px 350
                , fontFamily [ "Merriweather", "serif" ]

                --, getExtFont "FiraCode"
                , fontSize (px 4.9)
                , fontWeight FontWeightBold
                , fill <| Paint <| Color.rgb 0.5 0.5 0.5
                , edgeMode EdgeModeWrap
                ]
                [ text "The documentation for the Distributed Idea Mapping System (DIMS) will go here!" ]
            ]
-}
