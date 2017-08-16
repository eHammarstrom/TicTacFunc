module Components.HeaderView exposing (header)

import Html exposing (Html, div, text)
import Html.Attributes exposing (classList, class)
import Maybe
import Types exposing (..)


class_ : String -> ( String, Bool )
class_ x =
    ( x, True )


headerClasslist : Player -> List ( String, Bool )
headerClasslist p =
    [ class_ "header"
    , (case p of
        X ->
            class_ "header-player-x"

        O ->
            class_ "header-player-o"
      )
    ]


header : GameState -> Html (Maybe PlayerMove)
header gs =
    case gs.winner of
        None ->
            div [ classList (headerClasslist gs.playerTurn) ] [ text (playerToString gs.playerTurn) ]

        _ ->
            div [ class "header" ] []
