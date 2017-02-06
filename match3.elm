import Html exposing (Html, div)
import Svg exposing (Svg, svg, text, polygon, rect)
import Svg.Attributes exposing (version, x, y, viewBox, fill, points, width, height, transform)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { text : String
  }

init : (Model, Cmd Msg)
init =
  (Model "Hello world!", Cmd.none)


-- UPDATE

type Msg
  = Mark
  | Swap

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Mark ->
      (model, Cmd.none)

    Swap ->
      (model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ text model.text
    , logo
    ]


logo : Svg Msg
logo =
  svg
    [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95"
    ]
    [ polygon [ fill "#F0AD00", points "161.649,152.782 231.514,82.916 91.783,82.916" ] []
    , polygon [ fill "#7FD13B", points "8.867,0 79.241,70.375 232.213,70.375 161.838,0" ] []
    , rect
        [ fill "#7FD13B", x "192.99", y "107.392", width "107.676", height "108.167"
        , transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
        ]
        []
    , polygon [ fill "#60B5CC", points "323.298,143.724 323.298,0 179.573,0" ] []
    , polygon [ fill "#5A6378", points "152.781,161.649 0,8.868 0,314.432" ] []
    , polygon [ fill "#F0AD00", points "255.522,246.655 323.298,314.432 323.298,178.879" ] []
    , polygon [ fill "#60B5CC", points "161.649,170.517 8.869,323.298 314.43,323.298" ] []
    ]
