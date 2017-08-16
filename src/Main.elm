module Main exposing (..)

import Types exposing (..)
import Components.HeaderView exposing (header)
import Components.CheckerView exposing (checkerBoard)
import Html exposing (Html, beginnerProgram, div, text, button, br, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList)
import List exposing (member, concat, any, map, repeat, range, length)
import Tuple exposing (first, second)
import Maybe


-- LOGIC


playerPoints : Player -> GameState -> List Point
playerPoints p s =
    case p of
        X ->
            s.xs

        O ->
            s.os


isFull : GameState -> Bool
isFull s =
    if (s.size * s.size) == length (concat [ s.xs, s.os ]) then
        True
    else
        False


isFree : Point -> GameState -> Bool
isFree p s =
    not (member p (concat [ s.xs, s.os ]))


addTuple a b =
    ( first a + first b, second a + second b )


dirs =
    -- reversed y coords
    -- I miss list comprehensions
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

        traverseDir =
            traverseDirectionCheckWin size point ps
    in
        any (\x -> x == True) (map traverseDir dirs)


traverseDirectionCheckWin : Int -> Point -> List Point -> Direction -> Bool
traverseDirectionCheckWin ttl p ps d =
    let
        p_ =
            addTuple p d

        ttl_ =
            ttl - 1
    in
        case ttl_ of
            0 ->
                if member p ps then
                    True
                else
                    False

            _ ->
                if member p ps then
                    traverseDirectionCheckWin ttl_ p_ ps d
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


updatePs : GameState -> Point -> GameState
updatePs gs p =
    let
        playerTurn_ =
            nextPlayer gs.playerTurn

        newPlayerPoints =
            (playerPoints gs.playerTurn gs) ++ [ Debug.log "Updated to point" p ]
    in
        case gs.playerTurn of
            X ->
                Debug.log "GameState changed (X)" { gs | playerTurn = playerTurn_, xs = newPlayerPoints }

            O ->
                Debug.log "GameState changed (O)" { gs | playerTurn = playerTurn_, os = newPlayerPoints }


update :
    Maybe PlayerMove
    -> GameState
    -> GameState
update p gs =
    case p of
        Just (Move p) ->
            let
                gs_ =
                    updatePs gs p
            in
                if isWin gs.playerTurn p gs_ then
                    { gs_ | winner = (Winner gs.playerTurn) }
                else if isFull gs_ then
                    { gs_ | winner = Draw }
                else if isFree p gs then
                    gs_
                else
                    gs

        Just Restart ->
            initialState

        Nothing ->
            gs



-- VIEW


displayWinner : Player -> Html (Maybe PlayerMove)
displayWinner player =
    let
        playerClass =
            "winner-player-" ++ (String.toLower (toString player))
    in
        div [ class "winner", onClick (Just Restart) ]
            [ span [ class "winner-text" ] [ text "Winner!" ]
            , br [] []
            , span [ class playerClass ] [ text (toString player) ]
            ]


displayDraw : Html (Maybe PlayerMove)
displayDraw =
    div [ class "winner", onClick (Just Restart) ]
        [ span [ class "winner-text" ] [ text "Draw!" ] ]


view : GameState -> Html (Maybe PlayerMove)
view gs =
    div []
        [ header gs
        , div
            [ class "container" ]
            (case gs.winner of
                Winner x ->
                    [ displayWinner x ]

                Draw ->
                    [ displayDraw ]

                None ->
                    checkerBoard gs
            )
        ]


main =
    beginnerProgram { model = initialState, view = view, update = update }
