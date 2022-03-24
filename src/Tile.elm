module Tile exposing
    ( Biome(..)
    , Tile(..)
    , campFire
    , damageTile
    , ground
    , ore
    , rock
    )

import Color exposing (Color)


type Biome
    = Cold
    | Neutral
    | Fire


type Tile
    = Ground Color
    | Rock Color
    | Ore Color
    | CampFire


ground : Color -> Tile
ground biome =
    Ground biome


rock : Color -> Tile
rock biome =
    Rock biome


ore : Color -> Tile
ore biome =
    Ore biome


campFire : Tile
campFire =
    CampFire


damageTile : Maybe Tile -> Maybe Tile
damageTile tile =
    case tile of
        Just (Ground _) ->
            tile

        Just (Rock b) ->
            Just (Ground b)

        Just (Ore b) ->
            Just (Ground b)

        Just CampFire ->
            tile

        Nothing ->
            tile
