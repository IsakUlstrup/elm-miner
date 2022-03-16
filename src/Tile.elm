module Tile exposing (Tile(..), campFire, damageTile, ground, ore, rock)

import Range exposing (Range)


type Tile
    = Ground
    | Rock Destroyable
    | Ore Destroyable
    | CampFire


type alias Destroyable =
    { hp : Range Float
    , inventory : List Int
    }


destroyable : Float -> List Int -> Destroyable
destroyable hp inventory =
    Destroyable (Range.newRange 0 hp hp) inventory


ground : Tile
ground =
    Ground


rock : Float -> Tile
rock hp =
    Rock (destroyable hp [])


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
        Just Ground ->
            tile

        Just (Rock r) ->
            let
                ( newDest, _ ) =
                    damage dmg r
            in
            if Range.isEmpty newDest.hp then
                Just Ground

            else
                Just (Rock newDest)

        Just (Ore o) ->
            let
                ( newDest, _ ) =
                    damage dmg o
            in
            if Range.isEmpty newDest.hp then
                Just Ground

            else
                Just (Rock newDest)

        Just CampFire ->
            tile

        Nothing ->
            tile
