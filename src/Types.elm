module Types exposing (..)

-- MODEL


playerToString :
    Player
    -> String -- I miss type classes.
playerToString p =
    case p of
        X ->
            "X"

        O ->
            "O"


type Player
    = X
    | O


pointToString : Point -> String
pointToString p =
    let
        ( x, y ) =
            p
    in
        "(" ++ toString x ++ "," ++ toString y ++ ")"


type alias Point =
    ( Int, Int )


type alias GameState =
    { xs : List Point
    , os : List Point
    , size : Int
    , playerTurn : Player
    , winner : Maybe Player
    }


initialState =
    { xs = [], os = [], size = 3, playerTurn = X, winner = Nothing }


type alias Direction =
    Point


type PlayerMove
    = Move Point
    | Restart


type alias Winner =
    Maybe Player
