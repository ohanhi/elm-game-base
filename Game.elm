module Game exposing (..)

import Html exposing (Html, div, p, text)
import Html.App as Html
import Keyboard.Extra as Keyboard exposing (isPressed, Key(..))
import AnimationFrame
import Time exposing (Time)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { velocity : Float
    , position : Float
    , shotsFired : Int
    , keyModel : Keyboard.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( keyModel, keyCmd ) =
            Keyboard.init
    in
        ( { velocity = 0
          , position = 0
          , shotsFired = 0
          , keyModel = keyModel
          }
        , Cmd.map KeyMsg keyCmd
        )



-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyMsg Keyboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            updateModel dt model

        KeyMsg keyMsg ->
            updateKeys keyMsg model


updateModel : Time -> Model -> ( Model, Cmd Msg )
updateModel dt model =
    let
        arrows =
            Keyboard.arrows model.keyModel

        newModel =
            model
                |> updateVelocity (toFloat arrows.x)
                |> applyPhysics dt
    in
        ( newModel, Cmd.none )


updateKeys : Keyboard.Msg -> Model -> ( Model, Cmd Msg )
updateKeys keyMsg model =
    let
        ( newKeyModel, keyCmd ) =
            Keyboard.update keyMsg model.keyModel

        shotsFired =
            if not (isPressed Space model.keyModel) && isPressed Space newKeyModel then
                model.shotsFired + 1
            else
                model.shotsFired
    in
        ( { model
            | keyModel = newKeyModel
            , shotsFired = shotsFired
          }
        , Cmd.map KeyMsg keyCmd
        )


applyPhysics : Float -> Model -> Model
applyPhysics dt model =
    { model | position = model.position + model.velocity * dt }


updateVelocity : Float -> Model -> Model
updateVelocity newVelocity model =
    { model | velocity = newVelocity }


incrementShotsFired : Model -> Model
incrementShotsFired model =
    { model | shotsFired = model.shotsFired + 1 }



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ p [] [ text ("Velocity: " ++ toString model.velocity) ]
        , p [] [ text ("Position: " ++ toString model.position) ]
        , p [] [ text ("Shots fired: " ++ toString model.shotsFired) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
