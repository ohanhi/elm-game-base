module Game exposing (Model, Msg(..), applyPhysics, incrementShotsFired, init, initModel, keyDown, keyUp, main, subscriptions, update, updateVelocity, view)

import Browser
import Browser.Events as Events
import Debug
import Html exposing (Html, text)
import Key exposing (..)
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.element
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
    }


initModel : Model
initModel =
    { velocity = 0
    , position = 0
    , shotsFired = 0
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = TimeUpdate Float
    | KeyDown Key
    | KeyUp Key


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( applyPhysics dt model, Cmd.none )

        KeyDown key ->
            ( keyDown key model, Cmd.none )

        KeyUp key ->
            ( keyUp key model, Cmd.none )


keyDown : Key -> Model -> Model
keyDown key model =
    case key of
        Space ->
            incrementShotsFired model

        ArrowLeft ->
            updateVelocity -1.0 model

        ArrowRight ->
            updateVelocity 1.0 model

        _ ->
            model


keyUp : Key -> Model -> Model
keyUp key model =
    case key of
        ArrowLeft ->
            updateVelocity 0 model

        ArrowRight ->
            updateVelocity 0 model

        _ ->
            model


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
    text (Debug.toString model)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onAnimationFrameDelta TimeUpdate
        , Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Events.onKeyUp (Decode.map KeyUp keyDecoder)
        ]


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map fromCode (Decode.field "key" Decode.string)
