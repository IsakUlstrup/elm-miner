module Tile exposing (Tile(..), campFire, damage, ground, ore, rock)

import Range exposing (Range)


type Tile
    = Ground
    | Rock (Range Float)
    | Ore (Range Float)
    | CampFire


ground : Tile
ground =
    Ground


rock : Float -> Tile
rock hp =
    Rock (Range.newRange 0 hp hp)


ore : Float -> Tile
ore hp =
    Ore (Range.newRange 0 hp hp)


campFire : Tile
campFire =
    CampFire


damage : Float -> Maybe Tile -> Maybe Tile
damage dmg tile =
    case tile of
        Just Ground ->
            tile

        Just (Rock r) ->
            let
                newHp =
                    Range.subtract dmg r
            in
            if Range.isEmpty newHp then
                Just Ground

            else
                Just (Rock newHp)

        Just (Ore o) ->
            let
                newHp =
                    Range.subtract dmg o
            in
            if Range.isEmpty newHp then
                Just Ground

            else
                Just (Ore newHp)

        Just CampFire ->
            tile

        Nothing ->
            tile
