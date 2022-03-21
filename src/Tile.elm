module Tile exposing (Biome(..), Tile(..), campFire, damageTile, ground, ore, rock)

import Range exposing (Range)


type Biome
    = Cold
    | Neutral
    | Fire


type Tile
    = Ground Biome
    | Rock Destroyable Biome
    | Ore Destroyable
    | CampFire


type alias Destroyable =
    { hp : Range Float
    , inventory : List Int
    }


destroyable : Float -> List Int -> Destroyable
destroyable hp inventory =
    Destroyable (Range.newRange 0 hp hp) inventory


ground : Biome -> Tile
ground biome =
    Ground biome


rock : Float -> Biome -> Tile
rock hp biome =
    Rock (destroyable hp []) biome


ore : Float -> Tile
ore hp =
    Ore (destroyable hp [])


campFire : Tile
campFire =
    CampFire


damage : Float -> Destroyable -> ( Destroyable, Maybe (List Int) )
damage dmg dest =
    let
        newHp =
            Range.subtract dmg dest.hp
    in
    if Range.isEmpty newHp then
        ( { dest | hp = newHp }, Just dest.inventory )

    else
        ( { dest | hp = newHp }, Nothing )


damageTile : Float -> Maybe Tile -> Maybe Tile
damageTile dmg tile =
    case tile of
        Just (Ground _) ->
            tile

        Just (Rock r b) ->
            let
                ( newDest, _ ) =
                    damage dmg r
            in
            if Range.isEmpty newDest.hp then
                Just (Ground b)

            else
                Just (Rock newDest b)

        Just (Ore o) ->
            let
                ( newDest, _ ) =
                    damage dmg o
            in
            if Range.isEmpty newDest.hp then
                Just (Ground Neutral)

            else
                Just (Ore newDest)

        Just CampFire ->
            tile

        Nothing ->
            tile
