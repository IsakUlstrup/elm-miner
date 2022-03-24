module Tile exposing
    ( Tile(..)
    , campFire
    , damageTile
    , ground
    , ore
    , rock
    )

import Color exposing (Color)


type Tile
    = Ground Color
    | Rock Color
    | Ore Color
    | CampFire


ground : Color -> Tile
ground color =
    Ground color


rock : Color -> Tile
rock color =
    Rock color


ore : Color -> Tile
ore color =
    Ore color


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
