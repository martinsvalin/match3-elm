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
    , buffer : List Language
    }


type alias Grid =
    List (List Cell)


type alias Cell =
    { position : Position
    , language : Language
    , keep : Bool
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
    let
        width =
            9

        height =
            9

        buffer =
            List.range 1 100
                |> List.map getPseudoRandomLanguage

        allPositions =
            List.map
                (\column ->
                    List.map (\row -> ( column, row )) (List.range 1 height)
                )
                (List.range 1 width)
                |> List.concat

        initialCells =
            (List.take (width * height) buffer)
                |> (List.map cellFromLanguage)
                |> (zip allPositions)
                |> List.map (\( position, cell ) -> { cell | position = position })
    in
        { text = "Hello world!"
        , cells = initialCells
        , marked = Nothing
        , buffer = (List.drop (width * height) buffer)
        }


cellFromLanguage : Language -> Cell
cellFromLanguage lang =
    { language = lang
    , position = ( 0, 0 )
    , keep = True
    }


toGrid : (Cell -> Position) -> List Cell -> Grid
toGrid sorter cells =
    List.sortBy sorter cells
        |> chunk 9
        |> List.reverse


getPseudoRandomLanguage : Int -> Language
getPseudoRandomLanguage seed =
    let
        sinOfSeed =
            sin (toFloat seed)

        flooredSinOfSeed =
            floor (sinOfSeed * 10000)

        value =
            flooredSinOfSeed % 9
    in
        case value of
            0 ->
                JavaScript

            1 ->
                Ruby

            2 ->
                Elm

            3 ->
                Erlang

            4 ->
                Rust

            5 ->
                Clojure

            6 ->
                Scratch

            7 ->
                Swift

            8 ->
                Haskell

            _ ->
                Python



-- Sort cells for the grid so that positions are laid out in columns,
-- high row values top, low bottom.
-- Example: A 3x3 grid would look like this
-- [ [(1, 3), (2, 3), (3, 3)]
-- , [(1, 2), (2, 2), (3, 2)]
-- , [(1, 1), (2, 1), (3, 1)]
-- ]


columnSort : Cell -> Position
columnSort cell =
    let
        ( x, y ) =
            cell.position
    in
        ( y, x )


chunk : Int -> List a -> List (List a)
chunk size list =
    if (List.length list) > size then
        List.take size list :: chunk size (List.drop size list)
    else
        [ list ]


zip : List a -> List b -> List ( a, b )
zip xs ys =
    case ( xs, ys ) of
        ( x :: xBack, y :: yBack ) ->
            ( x, y ) :: zip xBack yBack

        ( _, _ ) ->
            []



-- UPDATE
-- The Msg type is a "union type", which is a fancy word for "it'll be one of these things"
-- There are the names of actions that can happen in our program.


type Msg
    = Mark Position
    | Swap Position Position
    | Clear
    | Fill



-- The update model is called whenever a message is sent (that is, whenever an action happens)
-- We combine the action and the current state of the world to produce the next state of the world


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mark position ->
            markPosition position model

        Swap pos1 pos2 ->
            update Clear { model | cells = (swap pos1 pos2 model.cells) }

        Clear ->
            update Fill { model | cells = (clear model.cells) }

        Fill ->
            ( { model | cells = (fill model.cells) }, Cmd.none )


markPosition : Position -> Model -> ( Model, Cmd Msg )
markPosition position model =
    case model.marked of
        Nothing ->
            ( { model | marked = Just position }, Cmd.none )

        Just mark ->
            if isAdjacent mark position then
                update (Swap mark position) { model | marked = Nothing }
            else
                ( { model | marked = Just position }, Cmd.none )


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



-- Swapping cells
-- 1. Identify the involved cells by Position. We need the cells to get their language.
-- 2. Go through cells. When you find one of the two we're looking for:
-- 3. Substitute language with the other cell's language (and vice versa)


swap : Position -> Position -> List Cell -> List Cell
swap pos1 pos2 cells =
    let
        cell1 =
            find_cell pos1 cells

        cell2 =
            find_cell pos2 cells
    in
        swapCell cells cell1 cell2


find_cell : Position -> List Cell -> Maybe Cell
find_cell position cells =
    List.filter (\cell -> cell.position == position) cells
        |> List.head


swapCell : List Cell -> Maybe Cell -> Maybe Cell -> List Cell
swapCell cells cell1 cell2 =
    case ( cell1, cell2 ) of
        ( Just one, Just two ) ->
            List.map (swapLanguageInCells one two) cells

        _ ->
            cells


swapLanguageInCells : Cell -> Cell -> Cell -> Cell
swapLanguageInCells one two current =
    if current == one then
        { current | language = two.language }
    else if current == two then
        { current | language = one.language }
    else
        current



-- Clearing cells
-- 1. Convert list of cells to a grid
-- 2. Looking for 3-in-a-row with the same language.
-- 3. Mark them.
-- 4. Then convert back to list. So far, we've only looked horizontally.
-- 5. Now convert to grid again, but with a different sort order.
-- 6. Do the same marking
-- 7. Convert back to list.
-- Now, every cell that is part of a 3-in-a-row pattern is marked.
-- 8. Filter out the marked cells.


clear : List Cell -> List Cell
clear cells =
    cells
        |> markForClearingHorizontal
        |> markForClearingVertical
        |> clearMarked


markForClearingHorizontal : List Cell -> List Cell
markForClearingHorizontal cells =
    cells |> (toGrid columnSort) |> markForClearing |> List.concat


markForClearingVertical : List Cell -> List Cell
markForClearingVertical cells =
    cells |> (toGrid .position) |> markForClearing |> List.concat


markForClearing : Grid -> Grid
markForClearing rows =
    List.map markInRow rows


markInRow : List Cell -> List Cell
markInRow cells =
    case cells of
        a :: b :: c :: rest ->
            if a.language == b.language && b.language == c.language then
                let
                    aa =
                        { a | keep = False }

                    bb =
                        { b | keep = False }

                    cc =
                        { c | keep = False }
                in
                    aa :: markInRow (aa :: bb :: rest)
            else
                a :: markInRow (b :: c :: rest)

        _ ->
            cells


clearMarked : List Cell -> List Cell
clearMarked cells =
    List.filter .keep cells



-- Filling cells
-- After clearing cells, we need new ones. The cells will be presented in a grid,
-- column-wise, where the first number in the position denotes the column.
-- Missing cells are filled in by adding more cells to that column.
-- To accomplish this, we look at the grid grouped by column.


fill : List Cell -> List Cell
fill cells =
    List.range 1 9
        |> (List.map
                (\column ->
                    ( column
                    , (pickColumn column cells)
                    )
                )
           )
        |> (List.map fillColumn)
        |> List.concat


pickColumn : Int -> List Cell -> List Cell
pickColumn column cells =
    List.filter
        (\cell ->
            let
                ( x, y ) =
                    cell.position
            in
                x == column
        )
        cells


fillColumn : ( Int, List Cell ) -> List Cell
fillColumn ( column, cells ) =
    let
        missing =
            9 - (List.length cells)

        positions =
            List.map (\row -> ( column, row )) (List.range 1 9)
                |> List.reverse
    in
        (cells ++ (newRandomCells missing))
            |> (List.sortBy .position)
            |> zip (positions)
            |> List.map (\( position, cell ) -> { cell | position = position })


newRandomCells : Int -> List Cell
newRandomCells n =
    case n of
        0 ->
            []

        n ->
            newRandomCell :: (newRandomCells (n - 1))


newRandomCell : Cell
newRandomCell =
    { position = ( 0, 0 )
    , language = Ruby
    , keep = True
    }



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
        , showGrid model.marked (toGrid columnSort model.cells)
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


showGrid : Maybe Position -> Grid -> Html Msg
showGrid mark rows =
    Html.div
        [ styles
            []
        ]
        (List.map (showRow mark) rows)


showRow : Maybe Position -> List Cell -> Html Msg
showRow mark cells =
    Html.div
        [ styles
            [ displayFlex
            ]
        ]
        (List.map (showCell mark) cells)


showCell : Maybe Position -> Cell -> Html Msg
showCell mark cell =
    Html.div
        [ styles
            [ backgroundColor (tintFor cell.language)
            , border (px 2)
            , borderColor (marked mark cell.position)
            , borderStyle solid
            , Css.width (px 64)
            , Css.height (px 64)
            , margin (px 3)
            , cursor pointer
            ]
        , onClick (Mark cell.position)
        ]
        [ Html.text (toString cell.position) ]


tintFor : Language -> Css.Color
tintFor language =
    case language of
        Elm ->
            rgb 135 202 65

        JavaScript ->
            rgb 247 223 30

        Ruby ->
            rgb 155 28 22

        Python ->
            rgb 54 113 161

        Erlang ->
            rgb 169 5 51

        Haskell ->
            rgb 102 73 142

        Swift ->
            rgb 253 131 54

        Clojure ->
            rgb 99 117 50

        Rust ->
            rgb 192 111 63

        Scratch ->
            rgb 141 88 211


marked : Maybe Position -> Position -> Css.Color
marked marked position =
    case marked of
        Nothing ->
            markedColor False

        Just marked ->
            markedColor (marked == position)


markedColor : Bool -> Css.Color
markedColor bool =
    if bool then
        rgb 255 0 0
    else
        rgb 0 0 0
