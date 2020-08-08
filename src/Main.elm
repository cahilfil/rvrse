module Main exposing (main)

import Browser
import Collage exposing (Collage, filled, ngon, rectangle, rotate, roundedRectangle, scale, uniform)
import Collage.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Collage.Layout exposing (debug, height, horizontal, impose, spacer, width)
import Collage.Render exposing (svg, svgExplicit)
import Color exposing (..)
import Html exposing (Attribute, div, node, text)
import Html.Attributes exposing (attribute, style)
import List.Extra
import Time


modelToCollage : Model -> Collage Msg
modelToCollage model =
    let
        polygonWithSpace i x a =
            let
                polygon =
                    ngon x 50 |> filled (uniform red) |> rotate (degrees a) |> onMouseEnter (\_ -> EnterPolygon i) |> onMouseLeave (\_ -> LeavePolygon i)
            in
            impose polygon (spacer 120 120)

        -- |> debug
        polygons =
            List.map3 polygonWithSpace (List.range 0 4) model.sides model.angles

        lineWithSpace i c =
            let
                c2 =
                    if c then
                        black

                    else
                        white

                line =
                    roundedRectangle 6 70 3
                        |> filled
                            (uniform
                                (case model.lineHovered of
                                    Just j ->
                                        if i == j then
                                            gray

                                        else
                                            c2

                                    Nothing ->
                                        c2
                                )
                            )
            in
            impose line (roundedRectangle 30 94 15 |> filled (uniform white)) |> onMouseEnter (\_ -> EnterLine i) |> onMouseLeave (\_ -> LeaveLine i) |> onClick (ClickLine i)

        lines =
            List.map2 lineWithSpace (List.range 0 3) model.linesClicked
    in
    List.Extra.interweave polygons lines |> horizontal



--main : Html msg


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { angles : List Float, sides : List Int, polygonHovered : Maybe Int, lineHovered : Maybe Int, linesClicked : List Bool }


type Msg
    = Tick
    | EnterPolygon Int
    | LeavePolygon Int
    | EnterLine Int
    | LeaveLine Int
    | ClickLine Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { angles = [ 0, 0, 0, 0, 0 ], sides = [ 4, 5, 6, 7, 8 ], polygonHovered = Nothing, lineHovered = Nothing, linesClicked = [ False, False, False, False ] }, Cmd.none )


view model =
    let
        gameCollage =
            model |> modelToCollage |> scale 1.2

        {- w = width gameCollage |> String.fromFloat

           h =
               height gameCollage |> String.fromFloat
        -}
        atCenter =
            List.map2 style [ "display", "align-items", "justify-content", "height" ] [ "flex", "center", "center", "100%" ]

        gameSvg =
            gameCollage |> svg

        --svgExplicit [ attribute "width" w, attribute "height" h ]
        --svgExplicit atCenter
        css =
            "html, body {height : 100%;}"
    in
    { title = "R V R S E"
    , body = [ node "style" [] [ text css ], div atCenter [ gameSvg ] ]
    }


update msg model =
    case msg of
        Tick ->
            let
                newAngles =
                    List.indexedMap
                        (\i a ->
                            let
                                speed =
                                    3

                                a2 =
                                    if a > speed then
                                        a - speed

                                    else
                                        0
                            in
                            case model.polygonHovered of
                                Nothing ->
                                    a2

                                Just j ->
                                    if i == j then
                                        a + 1

                                    else
                                        a2
                        )
                        model.angles
            in
            ( { model | angles = newAngles }, Cmd.none )

        EnterPolygon i ->
            ( { model | polygonHovered = Just i }, Cmd.none )

        LeavePolygon i ->
            ( { model | polygonHovered = Nothing }, Cmd.none )

        EnterLine i ->
            ( { model | lineHovered = Just i }, Cmd.none )

        LeaveLine i ->
            ( { model | lineHovered = Nothing }, Cmd.none )

        ClickLine i ->
            let
                newLineClicked =
                    List.indexedMap
                        (\j c ->
                            if i == j then
                                not c

                            else
                                c
                        )
                        model.linesClicked
            in
            ( { model | linesClicked = newLineClicked, lineHovered = Nothing }, Cmd.none )


subscriptions model =
    Time.every 10 (\_ -> Tick)
