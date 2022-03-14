module Player exposing (..)


type alias Player =
    { perception : Int
    , stamina : ( Float, Float )
    }


new : Player
new =
    Player 5 ( 10, 10 )


rest : Player -> Player
rest player =
    { player | stamina = ( Tuple.second player.stamina, Tuple.second player.stamina ) }


hasStamina : Player -> Bool
hasStamina player =
    Tuple.first player.stamina > 0


useStamina : Float -> Player -> Player
useStamina stam player =
    { player | stamina = ( Tuple.first player.stamina - stam, Tuple.second player.stamina ) }


staminaToString : Player -> String
staminaToString player =
    (Tuple.first player.stamina |> String.fromFloat) ++ "/" ++ (Tuple.second player.stamina |> String.fromFloat)
