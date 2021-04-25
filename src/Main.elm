module Main exposing (main)

import Browser
import Collage
    exposing
        ( Collage
        , filled
        , ngon
        , rectangle
        , rendered
        , rotate
        , roundedRectangle
        , scale
        , uniform
        )
import Collage.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Collage.Layout
    exposing
        ( align
        , center
        , debug
        , height
        , horizontal
        , impose
        , left
        , spacer
        , vertical
        , width
        )
import Collage.Render exposing (svg, svgExplicit)
import Collage.Text exposing (Shape(..), Typeface(..), color, fromString, huge, shape, size, typeface)
import Color
import Debug exposing (log)
import Html exposing (Attribute, Html, div, input, node, text)
import Html.Attributes exposing (attribute, style)
import Keyboard
import List.Extra
import Maybe.Extra
import Queue
import Random
import Random.List
import Time


numberOfObjects : Int
numberOfObjects =
    5


modelToCollage : Model a -> Collage Msg
modelToCollage model =
    let
        polygonWithSpace i x a c =
            let
                polygon =
                    ngon x 50 |> filled (uniform c) |> rotate (degrees a) |> onMouseEnter (\_ -> EnterPolygon i) |> onMouseLeave (\_ -> LeavePolygon i)
            in
            impose polygon (spacer 120 120)

        -- |> debug
        polygons =
            List.map4 polygonWithSpace (List.range 0 4) model.sides model.angles model.colors

        lineWithSpace i c =
            let
                c2 =
                    if c then
                        Color.black

                    else
                        Color.white

                line =
                    roundedRectangle 6 70 3
                        |> filled
                            (uniform
                                (case model.lineHovered of
                                    Just j ->
                                        if i == j then
                                            Color.gray

                                        else
                                            c2

                                    Nothing ->
                                        c2
                                )
                            )
            in
            impose line (roundedRectangle 30 94 15 |> filled (uniform Color.white)) |> onMouseEnter (\_ -> EnterLine i) |> onMouseLeave (\_ -> LeaveLine i) |> onClick (ClickLine i)

        lines =
            List.map2 lineWithSpace (List.range 0 3) model.linesClicked

        counter s n =
            s
                ++ ": "
                ++ String.fromInt n
                |> fromString
                |> typeface (Font "Text Me One")
                |> size huge
                |> color (Maybe.withDefault Color.red <| List.head model.colors)
                |> rendered

        winCounter =
            counter "wins" model.winCount

        lossCounter =
            counter "losses" model.lossCount

        moveCounter =
            counter "moves left" model.remainingMoves

        combined =
            case model.moveAngle of
                Just moveAngle ->
                    polygons
                        |> partition model.linesClicked
                        |> List.map
                            (\ps ->
                                let
                                    w =
                                        120 * List.length ps |> toFloat
                                in
                                impose
                                    (ps
                                        |> horizontal
                                        |> rotate (degrees -moveAngle)
                                        |> center
                                    )
                                    (spacer w 1)
                            )
                        |> horizontal
                        |> rotate (degrees moveAngle)

                Nothing ->
                    List.Extra.interweave polygons lines |> horizontal

        game =
            impose (center combined) (spacer 720 600) |> scale 1.2
    in
    [ winCounter
    , lossCounter
    , moveCounter
    , spacer 0 20
    , game
    ]
        |> List.map (align left)
        --|> List.map debug
        |> vertical



--main : Html msg


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model a =
    { angles : List Float
    , sides : List Int
    , colors : List Color.Color
    , polygonHovered : Maybe Int
    , lineHovered : Maybe Int
    , linesClicked : List Bool
    , pressedKeys : List Keyboard.Key
    , remainingMoves : Int
    , winCount : Int
    , lossCount : Int
    , permute : List a -> List a
    , moveAngle : Maybe Float
    }


type Msg
    = Tick
    | EnterPolygon Int
    | LeavePolygon Int
    | EnterLine Int
    | LeaveLine Int
    | ClickLine Int
    | KeyMsg Keyboard.Msg
    | Move
    | NewPermutation (List Int)
    | NewGame


init : () -> ( Model a, Cmd Msg )
init _ =
    ( { angles = [ 0, 0, 0, 0, 0 ]
      , sides = [ 4, 5, 6, 7, 8 ]
      , colors = [ Color.red, Color.yellow, Color.green, Color.blue, Color.purple ]
      , polygonHovered = Nothing
      , lineHovered =
            Nothing
      , linesClicked = [ False, False, False, False ]
      , pressedKeys = []
      , remainingMoves = -1
      , winCount = 0
      , lossCount = 0
      , permute = identity
      , moveAngle = Nothing
      }
    , List.range 0 4 |> Random.List.shuffle |> Random.generate NewPermutation
    )


view model =
    let
        gameCollage =
            model |> modelToCollage

        {- w = width gameCollage |> String.fromFloat

           h =
               height gameCollage |> String.fromFloat
        -}
        atCenter n =
            List.map2 style
                [ "display", "align-items", "justify-content", "height" ]
                [ "flex"
                , "center"
                , "center"
                , String.fromInt n ++ "%"
                ]

        gameSvg =
            gameCollage |> svg

        --svgExplicit [ attribute "width" w, attribute "height" h ]
        --svgExplicit atCenter
        css =
            "html, body {height : 90%;}"
    in
    div (atCenter 100) [ gameSvg ]


type alias Partition a =
    List (List a)


partition : List Bool -> List a -> Partition a
partition list1 list2 =
    case ( list1, list2 ) of
        ( _, [] ) ->
            []

        ( [], _ ) ->
            [ list2 ]

        ( x :: xs, y :: ys ) ->
            if x then
                [ y ] :: partition xs ys

            else
                case partition xs ys of
                    z :: zs ->
                        (y :: z) :: zs

                    [] ->
                        [ [ y ] ]


reversePermutation : List Bool -> List a -> List a
reversePermutation lines sides =
    let
        partitioned =
            log "partitioned" (partition lines sides)

        reversed =
            log "reversed" (List.reverse partitioned)
    in
    List.concat reversed


allPartitions : List a -> List (Partition a)
allPartitions list =
    case list of
        [] ->
            [ [] ]

        x :: xs ->
            let
                part =
                    allPartitions xs

                seperates =
                    List.map (\ys -> [ x ] :: ys) part

                joints =
                    part
                        |> List.map
                            (\ys ->
                                case ys of
                                    [] ->
                                        Nothing

                                    z :: zs ->
                                        Just ((x :: z) :: zs)
                            )
                        |> Maybe.Extra.values
            in
            seperates ++ joints


expand : ( List a, Int ) -> List ( List a, Int )
expand ( xs, dst ) =
    let
        perms =
            xs |> allPartitions |> List.map (List.reverse >> List.concat)
    in
    List.map (\ys -> ( ys, dst + 1 )) perms


dist : List Int -> List Int -> Int
dist src dest =
    let
        f q =
            case Queue.dequeue q of
                ( Just ( cur, d ), remQ ) ->
                    let
                        nxt =
                            expand ( cur, d )

                        newQ =
                            List.foldr Queue.enqueue remQ nxt
                    in
                    if cur == dest then
                        d

                    else
                        f newQ

                _ ->
                    -1

        --fix this
    in
    f <| Queue.enqueue ( src, 0 ) Queue.empty


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

                newModel =
                    { model | angles = newAngles }
            in
            case model.moveAngle of
                Nothing ->
                    ( newModel, Cmd.none )

                Just a ->
                    if a >= 180 then
                        update Move { newModel | moveAngle = Nothing }

                    else
                        ( { newModel | moveAngle = Just (a + 2) }
                        , Cmd.none
                        )

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

        KeyMsg keyMsg ->
            let
                newPressedKeys =
                    Keyboard.update keyMsg model.pressedKeys

                spacebarPressed =
                    List.member Keyboard.Spacebar newPressedKeys

                makeMove =
                    spacebarPressed && List.member True model.linesClicked

                newModel =
                    { model | pressedKeys = newPressedKeys }
            in
            if makeMove then
                ( { newModel | moveAngle = Just 0 }, Cmd.none )

            else
                ( newModel, Cmd.none )

        Move ->
            let
                movePermute =
                    reversePermutation model.linesClicked

                newLinesClicked =
                    [ False, False, False, False ]

                newRemainingMoves =
                    model.remainingMoves - 1

                newSides =
                    movePermute model.sides

                won =
                    newSides == [ 4, 5, 6, 7, 8 ]

                newModel =
                    { model
                        | sides = newSides
                        , angles = movePermute model.angles
                        , colors = movePermute model.colors
                        , linesClicked = newLinesClicked
                        , remainingMoves = newRemainingMoves
                    }
            in
            if won then
                ( { newModel | winCount = model.winCount + 1 }
                , List.range 0 4
                    |> Random.List.shuffle
                    |> Random.generate
                        NewPermutation
                )

            else if newRemainingMoves == 0 then
                update NewGame { newModel | lossCount = model.lossCount + 1 }

            else
                ( newModel, Cmd.none )

        NewPermutation p ->
            let
                newPermute xs =
                    List.Extra.zip p xs |> List.sortBy Tuple.first |> List.map Tuple.second
            in
            update NewGame { model | permute = newPermute }

        NewGame ->
            let
                ( newSides, ( newAngles, newColors ) ) =
                    List.map3 (\x y z -> ( x, ( y, z ) )) model.sides model.angles model.colors
                        |> List.sortBy Tuple.first
                        |> model.permute
                        |> List.unzip
                        |> Tuple.mapSecond List.unzip
            in
            ( { model
                | sides = newSides
                , angles = newAngles
                , colors = newColors
                , remainingMoves = dist newSides [ 4, 5, 6, 7, 8 ]
              }
            , Cmd.none
            )


subscriptions model =
    Sub.batch
        [ Time.every 10 (\_ -> Tick)
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
