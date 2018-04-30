module Main exposing (..)

import AnimationFrame
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard.Extra as Keys exposing (Key)
import Task
import Time exposing (Time)
import Window


-- MAIN


main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }



-- CONSTANTS


circle : Float
circle =
    pi * 2


skyImage : String
skyImage =
    "assets/deathvalley_panorama.jpg"



-- MODEL


type alias Model =
    { camera : Camera
    , direction : Direction
    , sky : Sky
    , pressedKeys : List Key
    }


type alias Camera =
    { width : Float
    , height : Float
    }


type alias Direction =
    Float


type alias Sky =
    { width : Float
    , height : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { pressedKeys = []
      , camera = initCamera (Window.Size 0 0)
      , direction = initDirection
      , sky = initSky (Window.Size 0 0)
      }
    , Task.perform WindowSize Window.size
    )


initCamera : Window.Size -> Camera
initCamera window =
    { width = toFloat window.width * 0.5
    , height = toFloat window.height * 0.5
    }


initDirection : Direction
initDirection =
    180


initSky : Window.Size -> Sky
initSky window =
    { width = 2000, height = toFloat window.height }



-- UPDATE


type Msg
    = WindowSize Window.Size
    | KeyMsg Keys.Msg
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowSize window ->
            ( { model | camera = initCamera window, sky = initSky window }, Cmd.none )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keys.update keyMsg model.pressedKeys }
            , Cmd.none
            )

        Tick delta ->
            let
                arrows =
                    Keys.arrows model.pressedKeys

                newDirection =
                    if arrows.x == -1 then
                        rotate model.direction (-pi * Time.inSeconds delta)
                    else if arrows.x == 1 then
                        rotate model.direction (pi * Time.inSeconds delta)
                    else
                        model.direction
            in
            ( { model | direction = newDirection }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    drawSky model.camera model.direction model.sky


drawSky : Camera -> Direction -> Sky -> Html Msg
drawSky camera direction sky =
    let
        width =
            sky.width * (camera.height / sky.height) * 2

        left =
            (direction / circle) * -width
    in
    div
        [ style
            [ ( "width", px (width*1.5) )
            , ( "height", px sky.height )
            , ( "overflow", "hidden" )
            , ( "background-image", "url('" ++ skyImage ++ "')" )
            , ( "background-repeat-x", "repeat" )
            , ( "background-repeat-y", "no-repeat" )
            , ( "background-position-x", px left )
            , ( "background-size", "cover" )
            ]
        ]
        []


px : Float -> String
px n =
    toString n ++ "px"


rotate : Float -> Direction -> Direction
rotate angle direction =
    direction + angle



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keys.subscriptions
        , Window.resizes WindowSize
        , AnimationFrame.diffs Tick
        ]
