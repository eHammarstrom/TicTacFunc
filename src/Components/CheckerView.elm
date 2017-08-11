module Components.CheckerView exposing (checkerBoard)

import Types exposing (..)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList)
import List exposing (member, concat, map, range)


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


checkerClasslist : Maybe Player -> Point -> List ( String, Bool )
checkerClasslist player p =
    let
        class_ =
            \x -> ( x, True )
    in
        concat
            [ colorChecker p
            , [ class_ "checker" ]
            , [ (case player of
                    Just X ->
                        class_ "player-x"

                    Just O ->
                        class_ "player-o"

                    Nothing ->
                        ( "", False )
                )
              ]
            ]


checker : Maybe Player -> Point -> Html (Maybe PlayerMove)
checker player p =
    div
        [ classList (checkerClasslist player p)
        , onClick (Just (Move p))
        ]
        [ text
            (case player of
                Just x ->
                    playerToString x

                Nothing ->
                    ""
            )
        ]


playerOnChecker : GameState -> Point -> Maybe Player
playerOnChecker gs p =
    if member p gs.xs then
        (Just X)
    else if member p gs.os then
        (Just O)
    else
        Nothing


checkerBoard : GameState -> List (Html (Maybe PlayerMove))
checkerBoard gs =
    let
        ss =
            range 0 (gs.size - 1)
    in
        (map
            (\x -> div [ class "row" ] (map (\y -> checker (playerOnChecker gs ( x, y )) ( x, y )) ss))
            ss
        )
