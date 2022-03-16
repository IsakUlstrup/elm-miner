module Player exposing (..)

import Range exposing (Range)


type alias Player =
    { perception : Int
    , stamina : Range Float
    , beer : Range Int
    }


new : Int -> Player
new perception =
    Player perception (Range.newRange 0 10 10) (Range.newRange 0 3 3)


rest : Player -> Player
rest player =
    { player
        | stamina = Range.setHigh player.stamina
        , beer = Range.setHigh player.beer
    }


hasStamina : Player -> Bool
hasStamina player =
    Range.isEmpty player.stamina |> not


hasBeer : Player -> Bool
hasBeer player =
    Range.isEmpty player.beer |> not


useStamina : Float -> Player -> Player
useStamina stam player =
    { player | stamina = Range.subtract stam player.stamina }


recoverStamina : Float -> Player -> Player
recoverStamina stam player =
    { player | stamina = Range.add stam player.stamina }


drinkBeer : Player -> Player
drinkBeer player =
    if hasBeer player then
        { player | beer = Range.subtract 1 player.beer }
            |> recoverStamina 3

    else
        player


staminaToString : Player -> String
staminaToString player =
    let
        stam =
            Range.viewHelperFloat player.stamina
    in
    stam.value ++ "/" ++ stam.max
