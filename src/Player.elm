module Player exposing (..)

import Item exposing (Item)
import Range exposing (Range)


type alias Player =
    { perception : Int
    , stamina : Range Float
    , beer : Range Int
    , damage : Float
    , inventory : List Item
    }


new : Int -> Float -> Player
new perception damage =
    Player perception (Range.newRange 0 10 10) (Range.newRange 0 5 5) damage []


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
            |> recoverStamina 6

    else
        player


lootItems : List Item -> Player -> Player
lootItems items player =
    { player | inventory = player.inventory ++ items }


staminaToString : Player -> String
staminaToString player =
    let
        stam =
            Range.viewHelperFloat player.stamina
    in
    stam.value ++ "/" ++ stam.max
