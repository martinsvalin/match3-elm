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


type alias ClearingState =
    { currentLanguage : Language
    , buffer : List Position
    , markedForDeletion : List Position
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


gridSize : Int
gridSize = 5

initModel : Model
initModel =
    { text = "Hello world!"
    , cells = generateCells gridSize gridSize
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
        if ((rowIndex + colIndex) % 3) == 0 then
            Elm
        else
            JavaScript
    }


toGrid : List Cell -> Grid
toGrid cells =
    List.sortBy .position cells
        |> chunk gridSize


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
    | Clear



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

                cells =
                    swap model.cells cell1 cell2
            in
                update Clear { model | cells = cells }

        Clear ->
            ( { model | cells = clear model.cells }, Cmd.none )


getFlippedPosition : Cell -> Position
getFlippedPosition {position} =
    case position of
        (x, y) -> (y, x)


clear : List Cell -> List Cell
clear cells =
    let
        verticallyOrdered = List.sortBy getFlippedPosition cells
        horizontallyOrdered = List.sortBy .position cells

        markedForDeletion = (clearOrdered verticallyOrdered) ++ (clearOrdered horizontallyOrdered)
    in
        fillWithTopOnes markedForDeletion cells


fillWithTopOnes : List Position -> List Cell -> List Cell
fillWithTopOnes positions cells =
    let
        columnsFromTheBottom = List.sortBy flipCellPositionAndNegateY cells
                                |> chunk gridSize
    in
        columnsFromTheBottom
            |> List.map (cleanColumn positions)
            |> List.concat


flipCellPositionAndNegateY : Cell -> Position
flipCellPositionAndNegateY cell =
    let
        (x, y) = cell.position
    in
        (x, (gridSize + 1) - y)


cleanColumn : List Position -> List Cell -> List Cell
cleanColumn positions column =
    (Debug.log "filtered column" (List.filter (isNotInList (Debug.log "positions" (positions))) (Debug.log "column" (column))))
        |> reindexColumn 1 []


reindexColumn : Int -> List Cell -> List Cell -> List Cell
reindexColumn index accumulator column =
    case column of
        [] ->
            accumulator

        x::xs ->
            reindexColumn (index + 1) (accumulator ++ [(reindexCell index x)]) xs


reindexCell : Int -> Cell -> Cell
reindexCell index cell =
    let
        (x,_) = cell.position
    in
        { cell | position = (x, index) }


isNotInList : List Position -> Cell -> Bool
isNotInList positions {position} =
    not (List.member position positions)


clearOrdered : List Cell -> List Position
clearOrdered cells =
    case cells of
        [] ->
            []

        (x::xs) ->
            let
                initialState =
                    { currentLanguage = x.language
                    , buffer = [x.position]
                    , markedForDeletion = []
                    }
            in
                List.foldl byLanguage initialState xs
                    |> .markedForDeletion


byLanguage : Cell -> ClearingState -> ClearingState
byLanguage { language, position } clearingState =
    if clearingState.currentLanguage == language then
        { clearingState | buffer = clearingState.buffer ++ [position] }
    else
        if List.length clearingState.buffer >= 3 then
            let
                stateMarkedForDeletion = (markBufferForDeletion clearingState)
            in
                { stateMarkedForDeletion | buffer = [position] , currentLanguage = language }
        else
            { clearingState | buffer = [position] , currentLanguage = language }


markBufferForDeletion : ClearingState -> ClearingState
markBufferForDeletion clearingState =
    { clearingState | markedForDeletion = clearingState.markedForDeletion ++ clearingState.buffer }


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
        , showGrid model model.cells
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


showGrid : Model -> List Cell -> Html Msg
showGrid model cells =
    Html.div
        [ styles
            []
        ]
        (List.map (showCell model) cells)


showCell : Model -> Cell -> Html Msg
showCell model cell =
    let
        (x, y) = cell.position
    in
        Html.div
            [ styles
                [ backgroundColor (tintFor cell.language)
                , border (px 2)
                , borderColor (marked model.marked cell.position)
                , borderStyle solid
                , Css.width (px 64)
                , Css.height (px 64)
                , position absolute
                , top (px (toFloat (x * 64)))
                , left (px (toFloat (y * 64)))
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

        Ruby ->
            rgb 155 28 22

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
