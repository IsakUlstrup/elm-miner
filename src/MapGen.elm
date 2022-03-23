module MapGen exposing (..)

import Random
import Tile exposing (Biome(..), Tile)


randomGround : Biome -> Random.Generator Tile
randomGround biome =
    Random.weighted
        ( 200, Tile.ground biome )
        [ ( 1, Tile.campFire )
        ]


randomRock : Biome -> Random.Generator Tile
randomRock biome =
    Random.weighted
        ( 100, Tile.rock biome )
        [ ( 10, Tile.ore biome )
        ]


tileType : Float -> Maybe Tile
tileType val =
    let
        biome v =
            let
                -- transform v from (-1 to 1) to (0 to 1)
                v2 =
                    (v + 1) / 2
            in
            -- select biome based on v2 and normalize v2 to (0 to 1) minus biome threshold
            if v2 < 1 / 3 then
                tile (v2 * 3) Cold

            else if v2 < 1 / 3 * 2 then
                tile ((v2 - (1 / 3)) * 3) Neutral

            else
                tile ((v2 - (1 / 3 * 2)) * 3) Fire

        tile v b =
            let
                pathWidth =
                    0.05
            in
            if v < (1 / 2 - pathWidth) || v > (1 / 2 + pathWidth) then
                Random.step (randomRock b) (Random.initialSeed (v * 1000 |> round)) |> Tuple.first

            else
                Random.step (randomGround b) (Random.initialSeed (v * 1000 |> round)) |> Tuple.first
    in
    Just (biome val)
