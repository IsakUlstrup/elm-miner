module MapGen exposing (..)

import Color exposing (Color, initColor, withHue, withSaturation)
import HexEngine.Point as Point exposing (Point)
import Random
import Tile exposing (Tile)


randomGround : Color -> Random.Generator Tile
randomGround biome =
    Random.weighted
        ( 50, Tile.ground biome )
        [ ( 1, Tile.campFire )
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
        deg =
            degToPoint point

        v2 =
            (val + 1) / 2

        dist =
            min 1 (Point.distanceFloat ( 0, 0, 0 ) point / 20)

        tile v =
            let
                pathWidth =
                    0.05

                color =
                    initColor |> withHue deg |> withSaturation (dist * 100 * v)
            in
            if v < (1 / 2 - pathWidth) || v > (1 / 2 + pathWidth) then
                Tile.rock color

            else
                generate point (randomGround color)
    in
    Just (tile v2)
