module Key exposing (..)


type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | Unknown


fromCode : String -> Key
fromCode keyCode =
    case keyCode of
        " " ->
            Space

        "ArrowLeft" ->
            ArrowLeft

        "ArrowRight" ->
            ArrowRight

        _ ->
            Unknown
