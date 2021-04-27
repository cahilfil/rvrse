module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
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
        , empty
        , height
        , horizontal
        , impose
        , left
        , spacer
        , topLeft
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
import Task
import Time


numberOfObjects : Int
numberOfObjects =
    5


modelToCollage : Model -> Collage Msg
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
            case model.remainingMoves of
                Just n ->
                    counter "moves left" n

                Nothing ->
                    empty

        combined =
            case model.moveTime of
                Just t ->
                    polygons
                        |> partition model.linesClicked
                        |> List.map (List.intersperse <| spacer 30 1)
                        |> List.map horizontal
                        --|> List.map debug
                        |> List.map
                            (\group ->
                                let
                                    w =
                                        group |> width
                                in
                                impose
                                    (group
                                        |> rotate (degrees <| toFloat model.direction * model.moveAngle)
                                        |> center
                                    )
                                    (spacer w 1)
                            )
                        |> List.intersperse
                            (impose
                                (spacer 30 1
                                    |> rotate (degrees <| toFloat model.direction * model.moveAngle)
                                    |> center
                                )
                                (spacer 30 1)
                            )
                        |> horizontal
                        |> rotate (degrees <| toFloat -model.direction * model.moveAngle)

                Nothing ->
                    case model.loadingTime of
                        Nothing ->
                            List.Extra.interweave polygons lines |> horizontal

                        Just lt ->
                            let
                                loadingTimes =
                                    [ 150, 300, 450, 600, 750 ]

                                isShown =
                                    List.map (\x -> x <= lt) loadingTimes

                                loadingPolygons =
                                    List.map2
                                        (\shown p ->
                                            if shown then
                                                p

                                            else
                                                spacer 120 0
                                        )
                                        isShown
                                        polygons
                            in
                            List.Extra.interweave loadingPolygons lines |> horizontal

        game =
            impose (center combined) (spacer 720 720)

        counters =
            [ spacer 10 0
            , [ spacer 0 10, winCounter, lossCounter, moveCounter ]
                |> List.map
                    (align
                        left
                    )
                |> vertical
            ]
                |> horizontal
    in
    impose (align topLeft counters) (align topLeft game)



--main : Html msg


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { angles : List Float
    , sides : List Int
    , colors : List Color.Color
    , polygonHovered : Maybe Int
    , lineHovered : Maybe Int
    , linesClicked : List Bool
    , pressedKeys : List Keyboard.Key
    , remainingMoves : Maybe Int
    , winCount : Int
    , lossCount : Int
    , moveAngle : Float
    , moveTime : Maybe Float
    , height : Maybe Float
    , width : Maybe Float
    , direction : Int
    , previousMoves : List (List Bool)
    , loadingTime : Maybe Float
    }


type Msg
    = Frame Float
    | EnterPolygon Int
    | LeavePolygon Int
    | EnterLine Int
    | LeaveLine Int
    | ClickLine Int
    | KeyMsg Keyboard.Msg
    | Move
    | NewGame (List Int)
    | GotViewport Browser.Dom.Viewport
    | WindowResize Int Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { angles = [ 0, 0, 0, 0, 0 ]
      , sides = [ 4, 5, 6, 7, 8 ]
      , colors = [ Color.red, Color.yellow, Color.green, Color.blue, Color.purple ]
      , polygonHovered = Nothing
      , lineHovered =
            Nothing
      , linesClicked = [ False, False, False, False ]
      , pressedKeys = []
      , remainingMoves = Nothing
      , winCount = 0
      , lossCount = 0
      , moveAngle = 0
      , moveTime = Nothing
      , width = Nothing
      , height = Nothing
      , direction = 1
      , previousMoves = []
      , loadingTime = Nothing
      }
    , Cmd.batch
        [ List.range 0 4 |> Random.List.shuffle |> Random.generate NewGame
        , Task.perform GotViewport Browser.Dom.getViewport
        ]
    )


view model =
    case ( model.width, model.height ) of
        ( Nothing, _ ) ->
            text ""

        ( _, Nothing ) ->
            text ""

        ( Just w, Just h ) ->
            let
                gameCollage =
                    model |> modelToCollage

                collageWidth =
                    width gameCollage

                collageHeight =
                    height gameCollage

                k =
                    min (w / collageWidth) (h / collageHeight)

                scaledGameCollage =
                    gameCollage |> scale k

                atCenter n =
                    List.map2 style
                        [ "display", "align-items", "justify-content", "height" ]
                        [ "flex"
                        , "center"
                        , "center"
                        , String.fromInt n ++ "%"
                        ]

                gameSvg =
                    scaledGameCollage |> svg

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
        Frame delta ->
            let
                newAngles =
                    List.indexedMap
                        (\i a ->
                            let
                                speed =
                                    0.3

                                a2 =
                                    if a > speed then
                                        a - delta * speed

                                    else
                                        0
                            in
                            case model.polygonHovered of
                                Nothing ->
                                    a2

                                Just j ->
                                    if i == j then
                                        a + delta * 0.1

                                    else
                                        a2
                        )
                        model.angles

                newLoadingTime =
                    case model.loadingTime of
                        Nothing ->
                            Nothing

                        Just lt ->
                            let
                                newLt =
                                    lt + delta
                            in
                            if newLt > 750 then
                                Nothing

                            else
                                Just newLt

                newModel =
                    { model | angles = newAngles, loadingTime = newLoadingTime }
            in
            case model.moveTime of
                Nothing ->
                    ( newModel, Cmd.none )

                Just t ->
                    let
                        newT =
                            t + delta

                        angle tme =
                            if tme <= 1000 then
                                180 / 1000 * tme

                            else
                                180

                        angle2 tme =
                            let
                                a2 =
                                    405 / (1000 * 1000)

                                t1 =
                                    1000 / 3

                                t2 =
                                    2000 / 3

                                vmax =
                                    270 / 1000
                            in
                            if t <= t1 then
                                a2 * t * t

                            else if t <= t2 then
                                45
                                    + vmax
                                    * (t - t1)

                            else if t <= 1000 then
                                135
                                    - a2
                                    * (t - t2)
                                    * (t - t2)
                                    + vmax
                                    * (t - t2)

                            else
                                180
                    in
                    if model.moveAngle >= 180 then
                        update Move { newModel | moveAngle = log "angle" 0, moveTime = Nothing }

                    else
                        ( { newModel
                            | moveAngle = angle2 newT
                            , moveTime =
                                --    log "time" <|
                                Just newT
                          }
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
            if makeMove && Maybe.Extra.isNothing model.moveTime then
                ( { newModel | moveTime = Just 0 }, Cmd.none )

            else
                ( newModel, Cmd.none )

        Move ->
            let
                movePermute =
                    reversePermutation model.linesClicked

                newLinesClicked =
                    if model.direction == 1 then
                        [ False, False, False, False ]

                    else
                        case model.previousMoves of
                            [] ->
                                [ False, False, False, False ]

                            m :: ms ->
                                List.reverse m

                newRemainingMoves =
                    Maybe.map (\x -> x - model.direction) model.remainingMoves

                newSides =
                    movePermute model.sides

                won =
                    newSides == [ 4, 5, 6, 7, 8 ]

                newPreviousMoves =
                    if model.direction == 1 then
                        model.linesClicked :: model.previousMoves

                    else
                        case model.previousMoves of
                            [] ->
                                []

                            m :: ms ->
                                ms

                newModel =
                    { model
                        | sides = newSides
                        , angles = movePermute model.angles
                        , colors = movePermute model.colors
                        , linesClicked = newLinesClicked
                        , remainingMoves = newRemainingMoves
                        , previousMoves = newPreviousMoves
                    }
            in
            if won then
                ( { newModel | winCount = model.winCount + 1 }
                , List.range 0 4
                    |> Random.List.shuffle
                    |> Random.generate
                        NewGame
                )

            else if Maybe.withDefault -1 newRemainingMoves == 0 then
                ( { newModel
                    | lossCount = model.lossCount + 1
                    , direction = -1
                    , moveTime = Just 0
                    , linesClicked = List.reverse model.linesClicked
                    , previousMoves = model.previousMoves
                  }
                , Cmd.none
                )

            else if model.direction == -1 then
                case model.previousMoves of
                    [] ->
                        ( { newModel | direction = 1 }, Cmd.none )

                    _ ->
                        ( { newModel | moveTime = Just 0 }, Cmd.none )

            else
                ( newModel, Cmd.none )

        NewGame p ->
            let
                permute xs =
                    List.Extra.zip p xs |> List.sortBy Tuple.first |> List.map Tuple.second

                ( newSides, ( newAngles, newColors ) ) =
                    List.map3 (\x y z -> ( x, ( y, z ) )) model.sides model.angles model.colors
                        |> List.sortBy Tuple.first
                        |> permute
                        |> List.unzip
                        |> Tuple.mapSecond List.unzip
            in
            ( { model
                | sides = newSides
                , angles = newAngles
                , colors = newColors
                , remainingMoves = Just <| dist newSides [ 4, 5, 6, 7, 8 ]
                , loadingTime = Just 0
              }
            , Cmd.none
            )

        GotViewport vp ->
            ( { model | width = Just vp.viewport.width, height = Just vp.viewport.height }, Cmd.none )

        WindowResize w h ->
            ( { model | width = Just <| toFloat w, height = Just <| toFloat h }, Cmd.none )


subscriptions model =
    Sub.batch
        [ --Time.every 1 (\_ -> Tick)
          Sub.map KeyMsg Keyboard.subscriptions
        , Browser.Events.onResize WindowResize
        , Browser.Events.onAnimationFrameDelta Frame
        ]
