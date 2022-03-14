module Player exposing (..)


type alias Player =
    { perception : Int
    , stamina : ( Float, Float )
    , beer : ( Int, Int )
    }


new : Player
new =
    Player 5 ( 10, 10 ) ( 3, 3 )


rest : Player -> Player
rest player =
    { player
        | stamina = ( Tuple.second player.stamina, Tuple.second player.stamina )
        , beer = ( Tuple.second player.beer, Tuple.second player.beer )
    }


hasStamina : Player -> Bool
hasStamina player =
    Tuple.first player.stamina > 0


useStamina : Float -> Player -> Player
useStamina stam player =
    { player | stamina = ( max 0 (Tuple.first player.stamina - stam), Tuple.second player.stamina ) }


recoverStamina : Float -> Player -> Player
recoverStamina stam player =
    { player | stamina = ( min (Tuple.second player.stamina) (Tuple.first player.stamina + stam), Tuple.second player.stamina ) }


drinkBeer : Player -> Player
drinkBeer player =
    if Tuple.first player.beer > 0 then
        { player | beer = ( Tuple.first player.beer - 1, Tuple.second player.beer ) }
            |> recoverStamina 3

    else
        player


hasBeer : Player -> Bool
hasBeer player =
    Tuple.first player.beer > 0


staminaToString : Player -> String
staminaToString player =
    (Tuple.first player.stamina |> String.fromFloat) ++ "/" ++ (Tuple.second player.stamina |> String.fromFloat)
