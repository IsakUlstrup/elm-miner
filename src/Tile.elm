module Tile exposing
    ( Biome(..)
    , Tile(..)
    , campFire
    , damageTile
    , ground
    , ore
    , rock
    )


type Biome
    = Cold
    | Neutral
    | Fire


type Tile
    = Ground Biome
    | Rock Biome
    | Ore Biome
    | CampFire


ground : Biome -> Tile
ground biome =
    Ground biome


rock : Biome -> Tile
rock biome =
    Rock biome


ore : Biome -> Tile
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
