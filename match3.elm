-- imports always go on top


module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css exposing (..)


-- this main function can be pretty much the same every time. not all programs will be Html.program, but lots will.
-- so don't worry about it and just copy-paste it :)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL
-- Model is as good a name as any for the data that describes your world. Feel free to re-use the name.
-- We will stick everything we need to keep track of here.


type alias Model =
    { text : String
    , cells : List Cell
    , marked : Maybe Position
    }


type alias Grid =
    List (List Cell)


type alias Cell =
    { position : Position
    , language : Language
    }


type alias Position =
    ( Int, Int )


type Language
    = JavaScript
    | Ruby
    | Python
    | Erlang
    | Elm
    | Swift
    | Clojure
    | Rust
    | Haskell
    | Scratch



-- The model needs a starting value. The program gets an initial value by calling init
-- The Cmd is there for things like making web requests, getting random numbers, making sound etc.


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { text = "Hello world!"
    , cells = generateCells 9 9
    , marked = Nothing
    }


generateCells : Int -> Int -> List Cell
generateCells n m =
    List.map (generateRow m) (List.range 1 n)
        |> List.concat


generateRow : Int -> Int -> List Cell
generateRow m rowIndex =
    List.map (generateCell rowIndex) (List.range 1 m)


generateCell : Int -> Int -> Cell
generateCell rowIndex colIndex =
    { position = ( rowIndex, colIndex )
    , language =
        if ((rowIndex + colIndex) % 2) == 0 then
            Elm
        else
            JavaScript
    }


toGrid : List Cell -> Grid
toGrid cells =
    List.sortBy .position cells
        |> chunk 9


chunk : Int -> List a -> List (List a)
chunk size list =
    if (List.length list) > size then
        List.take size list :: chunk size (List.drop size list)
    else
        [ list ]



-- UPDATE
-- The Msg type is a "union type", which is a fancy word for "it'll be one of these things"
-- There are the names of actions that can happen in our program.


type Msg
    = Mark Position
    | Swap Position Position



-- The update model is called whenever a message is sent (that is, whenever an action happens)
-- We combine the action and the current state of the world to produce the next state of the world


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mark position ->
            case model.marked of
                Nothing ->
                    ( { model | marked = Just position }, Cmd.none )

                Just marked ->
                    if isAdjacent marked position then
                        update (Swap marked position) { model | marked = Nothing }
                    else
                        ( { model | marked = Just position }, Cmd.none )

        Swap pos1 pos2 ->
            let
                cell1 =
                    find_cell pos1 model.cells

                cell2 =
                    find_cell pos2 model.cells
            in
                ( { model | cells = (swap model.cells cell1 cell2) }, Cmd.none )


isAdjacent : Position -> Position -> Bool
isAdjacent source target =
    List.member target (adjacents source)


adjacents : Position -> List Position
adjacents ( x, y ) =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    ]


swap : List Cell -> Maybe Cell -> Maybe Cell -> List Cell
swap cells cell1 cell2 =
    case ( cell1, cell2 ) of
        ( Just one, Just two ) ->
            List.map (swap_cell one two) cells

        _ ->
            cells


swap_cell : Cell -> Cell -> Cell -> Cell
swap_cell one two current =
    if current == one then
        { current | language = two.language }
    else if current == two then
        { current | language = one.language }
    else
        current


find_cell : Position -> List Cell -> Maybe Cell
find_cell position cells =
    List.filter (\cell -> cell.position == position) cells
        |> List.head



-- SUBSCRIPTIONS
-- No subscriptions so far. We could subscribe to stuff like keyboard events,
-- mouse events, changes to the browser location bar, etc.


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
-- Every time the model is updated, the new model gets passed to view, where it
-- is rendered to something the browser can display.
-- So view takes in a model, and turns it into Html for the Html.program to show.
-- We can write Html straight in Elm, in a similar way to how normal Html is written.
-- The `div` below is an Elm function, but it represents an Html <div> tag,
-- Every one of these tag functions take two lists, first the attributes, and then
-- whatever is inside the tag – the children.


view : Model -> Html Msg
view model =
    Html.div
        [ styles
            [ backgroundColor (hex "#FFFFFF")
            , Css.width (px 630)
            , Css.marginLeft auto
            , Css.marginRight auto
            ]
        ]
        [ header model.text
        , showGrid model (toGrid model.cells)
        ]


header : String -> Html Msg
header text =
    Html.h1
        [ styles
            [ textAlign center
            , margin (px 0)
            , padding (px 20)
            , fontFamily monospace
            ]
        ]
        [ Html.text text ]


styles : List Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


showGrid : Model -> Grid -> Html Msg
showGrid model rows =
    Html.div
        [ styles
            []
        ]
        (List.map (showRow model) rows)


showRow : Model -> List Cell -> Html Msg
showRow model cells =
    Html.div
        [ styles
            [ displayFlex
            ]
        ]
        (List.map (showCell model) cells)


showCell : Model -> Cell -> Html Msg
showCell model cell =
    Html.div
        [ styles
            [ backgroundColor (tintFor cell.language)
            , border (px 2)
            , borderColor (marked model.marked cell.position)
            , borderStyle solid
            , Css.width (px 64)
            , Css.height (px 64)
            , margin (px 3)
            , cursor pointer
            ]
        , onClick (Mark cell.position)
        ]
        []


tintFor : Language -> Css.Color
tintFor language =
    case language of
        Elm ->
            rgb 135 202 65

        JavaScript ->
            rgb 247 223 30

        _ ->
            rgb 0 0 0


marked : Maybe Position -> Position -> Css.Color
marked marked position =
    case marked of
        Nothing ->
            markedColor False

        Just marked ->
            markedColor (marked == position)


markedColor : Bool -> Css.Color
markedColor bool =
    case bool of
        True ->
            rgb 255 0 0

        False ->
            rgb 0 0 0
