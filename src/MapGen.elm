module MapGen exposing (..)

import Color exposing (Color, initColor, withHue, withLightness, withSaturation)
import HexEngine.Point as Point exposing (Point)
import Random
import Tile exposing (Biome(..), Tile, ground)


randomGround : Color -> Random.Generator Tile
randomGround biome =
    Random.weighted
        ( 200, Tile.ground biome )
        [ ( 1, Tile.campFire )
        ]


randomRock : Color -> Random.Generator Tile
randomRock biome =
    Random.weighted
        ( 100, Tile.rock biome )
        [ ( 10, Tile.ore biome )
        ]


tileType : ( Point, Float ) -> Maybe Tile
tileType ( point, val ) =
    let
        biome v =
            let
                -- transform v from (-1 to 1) to (0 to 1)
                v2 =
                    (v + 1) / 2
            in
            -- select biome based on v2 and normalize v2 to (0 to 1) minus biome threshold
            if v2 < 1 / 3 then
                tile (v2 * 3) (initColor |> withHue 100)

            else if v2 < 1 / 3 * 2 then
                tile ((v2 - (1 / 3)) * 3) (initColor |> withHue 200)

            else
                tile ((v2 - (1 / 3 * 2)) * 3) (initColor |> withHue 300)

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


rainbow : ( Point, Float ) -> Maybe Tile
rainbow ( point, val ) =
    let
        ( nx, ny, nz ) =
            Point.normalize point

        ( xa, za, ya ) =
            ( 30, 90, 150 )

        deg =
            nx * xa + ny * ya + nz * za

        v2 =
            (val + 1) / 2

        tile v b =
            let
                pathWidth =
                    0.05
            in
            if v < (1 / 2 - pathWidth) || v > (1 / 2 + pathWidth) then
                -- Random.step (randomRock b) (Random.initialSeed (v * 1000 |> round)) |> Tuple.first
                Tile.rock b

            else
                Random.step (randomGround b) (Random.initialSeed (v * 1000 |> round)) |> Tuple.first

        color v =
            if v < 0.5 then
                initColor |> withSaturation 0 |> withLightness 100

            else
                initColor |> withHue (deg * 2) |> withSaturation (v2 * 100)
    in
    Just (tile v2 (color v2))
