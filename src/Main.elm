module Main exposing (..)

import Html exposing (Html, beginnerProgram, div, text, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList)
import List exposing (member, concat, any, map, repeat, range)
import Tuple exposing (first, second)


-- MODEL


type Player
    = X
    | O


type alias Point =
    ( Int, Int )


type alias GameState =
    { xs : List Point
    , os : List Point
    , size : Int
    , playerTurn : Player
    }


initialState =
    { xs = [], os = [], size = 3, playerTurn = X }


playerPoints : Player -> GameState -> List Point
playerPoints p s =
    case p of
        X ->
            s.xs

        O ->
            s.os


isFree : Point -> GameState -> Bool
isFree p s =
    member p (concat [ s.xs, s.os ])


type alias Direction =
    Point



-- reversed y coords


addTuple a b =
    ( first a + first b, second a + second b )


dirs =
    -- I miss list comprehensions/applicatives
    [ ( 0, -1 ) -- N
    , ( -1, 0 ) -- W
    , ( 0, 1 ) -- S
    , ( 1, 0 ) -- E
    , ( -1, -1 ) -- NW
    , ( -1, 1 ) -- SW
    , ( 1, 1 ) -- SE
    , ( 1, -1 ) -- NE
    ]


isWin : Player -> Point -> GameState -> Bool
isWin player point state =
    let
        size =
            state.size

        ps =
            playerPoints player state

        traverse =
            traverseCheckWin size point ps
    in
        any (\x -> x == True) (map traverse dirs)


type alias TTL =
    Int


traverseCheckWin : TTL -> Point -> List Point -> Direction -> Bool
traverseCheckWin ttl p ps d =
    let
        p_ =
            ( first p + first d, second p + second d )

        ttl_ =
            ttl - 1
    in
        case ttl_ of
            0 ->
                if member p_ ps then
                    True
                else
                    False

            _ ->
                if member p_ ps then
                    True || traverseCheckWin ttl_ p_ ps d
                else
                    False


nextPlayer : Player -> Player
nextPlayer p =
    case p of
        X ->
            O

        O ->
            X



-- UPDATE


type PlayerMove
    = Move Point


update :
    PlayerMove
    -> GameState
    -> GameState -- msg : player move, model : game state
update (Move p) gs =
    let
        playerTurn_ =
            nextPlayer gs.playerTurn

        newPlayerPoints =
            (playerPoints gs.playerTurn gs) ++ [ p ]
    in
        if isFree p gs then
            case gs.playerTurn of
                X ->
                    { gs | playerTurn = playerTurn_, xs = newPlayerPoints }

                O ->
                    { gs | playerTurn = playerTurn_, os = newPlayerPoints }
        else
            gs



-- VIEW


isEven x =
    (x % 2) == 0


colorChecker : Point -> List ( String, Bool )
colorChecker p =
    let
        ( x, y ) =
            p
    in
        [ ( "black", (isEven x && isEven y) )
        , ( "white", (isEven x && not (isEven y)) )
        , ( "black", (not (isEven x) && not (isEven y)) )
        , ( "white", (not (isEven x) && isEven y) )
        ]


checker : Point -> Html PlayerMove
checker p =
    let
        ( x, y ) =
            p
    in
        div
            [ classList (concat [ colorChecker p, [ ( "checker", True ) ] ])
            , onClick (Move ( 0, 0 ))
            ]
            [ text ("(" ++ toString x ++ "," ++ toString y ++ ")") ]


view : GameState -> Html PlayerMove
view gs =
    let
        ss =
            range 0 (gs.size - 1)
    in
        div [ class "container" ]
            (map
                (\x -> div [ class "row" ] (map (\y -> checker ( x, y )) ss))
                ss
            )



--(repeat gs.size (div [ class "row" ] (repeat gs.size (checker ( 0, 0 )))))


main =
    beginnerProgram { model = initialState, view = view, update = update }
