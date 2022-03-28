module MapGen exposing (..)

import Color exposing (Color, initColor, withHue, withLightness, withSaturation)
import HexEngine.Point as Point exposing (Point)
import Random
import Tile exposing (Tile)


randomGround : Color -> Random.Generator Tile
randomGround biome =
    Random.weighted
        ( 50, Tile.ground biome )
        [ ( 2, Tile.campFire )
        ]


randomRock : Color -> Random.Generator Tile
randomRock biome =
    Random.weighted
        ( 100, Tile.rock biome )
        [ ( 10, Tile.ore biome )
        ]


generate : Point -> Random.Generator Tile -> Tile
generate point gen =
    Random.step gen (Random.initialSeed (Point.toInt point)) |> Tuple.first


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

        tile v c =
            let
                pathWidth =
                    0.05
            in
            if v < (1 / 2 - pathWidth) || v > (1 / 2 + pathWidth) then
                generate point (randomRock c)

            else
                generate point (randomGround c)
    in
    Just (biome val)


degToPoint : Point -> Float
degToPoint point =
    let
        ( q, r ) =
            Point.toAxial point
    in
    (atan2 -(1 * (sqrt 3 * toFloat q + sqrt 3 / 2 * toFloat r))
        -(1 * (3 / 2 * toFloat r))
        * 180
        / pi
    )
        + 180


rainbow : ( Point, Float ) -> Maybe Tile
rainbow ( point, val ) =
    let
        -- deg =
        --     degToPoint point
        -- transform (-1 to 1) to (0 to 1)
        normV =
            (val + 1) / 2

        -- dist =
        --     min 1 (Point.distanceFloat ( 0, 0, 0 ) point / 20)
        -- how far is v from a?
        -- distFromAngle a v =
        --     (v - a)
        --         |> abs
        --         |> (\c -> c / a)
        pathWidth =
            0.04

        color v a =
            if a < 120 then
                -- Yellow
                -- initColor |> withHue 180 |> withSaturation ((100 - (distFromAngle 60 a * 100)) * v)
                initColor |> withHue 60 |> withSaturation (100 * v)

            else if a < 240 then
                -- Cyan
                -- initColor |> withHue 300 |> withSaturation ((100 - (distFromAngle 270 a * 100)) * v)
                initColor |> withHue 180 |> withSaturation (100 * v)

            else
                -- Magenta
                -- initColor |> withHue 50 |> withSaturation ((100 - (distFromAngle 250 deg * 100)) * v)
                initColor |> withHue 300 |> withSaturation (100 * v)

        tile value angle =
            if value > (1 / 2 - pathWidth) && value < (1 / 2 + pathWidth) then
                -- ground
                generate point (randomGround (color value angle))

            else if value > 0.8 then
                -- ore
                Tile.ore (color value angle)

            else
                -- rock
                Tile.rock (color value angle)
    in
    Just (tile normV (degToPoint point))
