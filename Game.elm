module Game exposing (..)

import Html exposing (Html, text)
import Html.App as Html
import Keyboard.Extra as Keyboard
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
    , shooting : Bool
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
          , shooting = False
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
            updateKeyModel keyMsg model


updateModel : Time -> Model -> ( Model, Cmd Msg )
updateModel dt model =
    let
        arrows =
            Keyboard.arrows model.keyModel

        newModel =
            model
                |> updateShooting
                |> updateVelocity (toFloat arrows.x)
                |> applyPhysics dt
    in
        ( newModel, Cmd.none )


updateKeyModel : Keyboard.Msg -> Model -> ( Model, Cmd Msg )
updateKeyModel keyMsg model =
    let
        ( keyModel, keyCmd ) =
            Keyboard.update keyMsg model.keyModel

        newModel =
            { model | keyModel = keyModel }
    in
        ( newModel, Cmd.map KeyMsg keyCmd )


applyPhysics : Float -> Model -> Model
applyPhysics dt model =
    { model | position = model.position + model.velocity * dt }


updateVelocity : Float -> Model -> Model
updateVelocity newVelocity model =
    { model | velocity = newVelocity }


updateShooting : Model -> Model
updateShooting model =
    { model | shooting = Keyboard.isPressed Keyboard.Space model.keyModel }



-- VIEW


view : Model -> Html msg
view model =
    text (toString model)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
