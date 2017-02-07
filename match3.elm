-- imports always go on top
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
  { text : String,
    grid : List (List Cell)
  }

type alias Cell =
  { color : String
  , marked : Bool
  , position : Position
  }

type alias Position = (Int, Int)

-- The model needs a starting value. The program gets an initial value by calling init
init : (Model, Cmd Msg)
init =
  (initModel, Cmd.none)


initModel : Model
initModel =
  { text = "Hello world!"
  , grid = generateGrid 5 5
  }

generateGrid : Int -> Int -> List (List Cell)
generateGrid n m =
  List.map (generateRow m) (List.range 0 n)

generateRow : Int -> Int -> List Cell
generateRow m rowIndex =
  List.map (generateCell rowIndex) (List.range 0 m)

generateCell : Int -> Int -> Cell
generateCell rowIndex colIndex =
  { color = "#ff0"
  , marked = False
  , position = (rowIndex, colIndex)
  }


-- UPDATE

-- The Msg type is a "union type", which is a fancy word for "it'll be one of these things"
-- There are the names of actions that can happen in our program.
type Msg
  = Mark Position
  | Swap


-- The update model is called whenever a message is sent (that is, whenever an action happens)
-- We combine the action and the current state of the world to produce the next state of the world
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Mark position ->
      ({model | grid = generateGrid 10 10}, Cmd.none)

    Swap ->
      (model, Cmd.none)


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
    []
    [ Html.text model.text
    , showGrid model.grid
    ]

styles : List Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style

showGrid : List (List Cell) -> Html Msg
showGrid rows =
  Html.table
    []
    (List.map showRow rows)

showRow : List Cell -> Html Msg
showRow cells =
  Html.tr
    []
    (List.map showCell cells)

showCell : Cell -> Html Msg
showCell cell =
  Html.td
    [ styles [backgroundColor (hex cell.color)]
    , onClick (Mark cell.position)
    ]
    [Html.text "cell"]
