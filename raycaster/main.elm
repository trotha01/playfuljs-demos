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


(skyImage, knifeHand) =
    ("assets/deathvalley_panorama.jpg"
    ,"assets/knife_hand.png"
    )

(knifeHandWidth, knifeHandHeight) =
    (319, 320)


-- MODEL


type alias Model =
    { camera : Camera
    , direction : Direction
    , sky : Sky
    , paces : Float
    , pressedKeys : List Key
    , pause : Bool
    }


type alias Camera =
    { width : Float
    , height : Float
    , scale : Float
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
      , paces = 0
      , sky = initSky (Window.Size 0 0)
      , pause = False
      }
    , Task.perform WindowSize Window.size
    )


initCamera : Window.Size -> Camera
initCamera window =
    { width = toFloat window.width * 0.5
    , height = toFloat window.height * 0.5
    , scale = (toFloat window.width *0.5 + toFloat window.height *0.5) / 1200
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
                pause = if (List.any ((==) Keys.Space) model.pressedKeys)
                  then not model.pause
                  else model.pause
                arrows =
                    Keys.arrows model.pressedKeys

                newDirection =
                    if arrows.x == -1 then
                        rotate model.direction (-pi * Time.inSeconds delta)
                    else if arrows.x == 1 then
                        rotate model.direction (pi * Time.inSeconds delta)
                    else
                        model.direction
                newPaces =
                    if arrows.y == -1 then
                         walk (-3 * Time.inSeconds delta)
                    else if arrows.y == 1 then
                         walk (3 * Time.inSeconds delta)
                    else
                        0
            in
            ( { model | direction = newDirection, paces = model.paces + newPaces, pause = pause }
            , Cmd.none
            )


walk : Float -> Float
walk distance =
     distance

rotate : Float -> Direction -> Direction
rotate angle direction =
    direction + angle


-- VIEW


view : Model -> Html Msg
view model =
    div []
    [ viewSky model.camera model.direction model.sky
    , viewWeapon model.camera model.paces
    ]

viewWeapon : Camera ->  Float -> Html msg
viewWeapon camera paces =
    let
        bobX = cos (paces * 2) * camera.scale * 6
        bobY = sin (paces * 4) * camera.scale * 6
    in div
    [ style
        [ ("position", "absolute")
        , ("bottom", px (0 - bobY))
        , ("right", px (0 - bobX))
        , ("width", px (knifeHandWidth * camera.scale))
        , ("height", px (knifeHandHeight * camera.scale))
        , ("background-image", "url('" ++ knifeHand ++ "')")
        , ("background-repeat", "no-repeat")
        ]
    ] []

viewSky : Camera -> Direction -> Sky -> Html Msg
viewSky camera direction sky =
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.pause then
       Sub.batch
        [ Sub.map KeyMsg Keys.subscriptions
        , Window.resizes WindowSize
        ]
    else
       Sub.batch
        [ Sub.map KeyMsg Keys.subscriptions
        , Window.resizes WindowSize
        , AnimationFrame.diffs Tick
        ]
