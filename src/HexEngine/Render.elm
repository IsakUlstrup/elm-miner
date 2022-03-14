module HexEngine.Render exposing (cornersToString, fancyHexCorners, pointToPixel, renderGrid)

import Dict
import HexEngine.Grid exposing (HexGrid)
import HexEngine.Point as Point exposing (Point)
import HexEngine.RandomMap exposing (RandomMap)
import Svg exposing (Svg, g, svg)
import Svg.Attributes
import Svg.Keyed
import Svg.Lazy


{-| Hex size constant
-}
hexSize : Float
hexSize =
    5


{-| Get the center of a given point in screen coordinates
-}
pointToPixel : Bool -> Point -> ( Float, Float )
pointToPixel flatTop point =
    let
        ( q, r ) =
            Point.toAxial point
    in
    if flatTop then
        ( hexSize * (3 / 2 * toFloat q)
        , hexSize * (sqrt 3 / 2 * toFloat q + sqrt 3 * toFloat r)
        )

    else
        ( hexSize * (sqrt 3 * toFloat q + sqrt 3 / 2 * toFloat r)
        , hexSize * (3 / 2 * toFloat r)
        )


cornersToString : HexCorners -> String
cornersToString { c0, c1, c2, c3, c4, c5 } =
    let
        tupleToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    tupleToString c0
        ++ " "
        ++ tupleToString c1
        ++ " "
        ++ tupleToString c2
        ++ " "
        ++ tupleToString c3
        ++ " "
        ++ tupleToString c4
        ++ " "
        ++ tupleToString c5


{-| The six corners of a hex in screen coordinates
-}
type alias HexCorners =
    { c0 : ( Float, Float )
    , c1 : ( Float, Float )
    , c2 : ( Float, Float )
    , c3 : ( Float, Float )
    , c4 : ( Float, Float )
    , c5 : ( Float, Float )
    }



---- EXPERIMENTAL ----


{-| Calculate hex corners in screen coordinates
-}
fancyHexCorners : Bool -> HexCorners
fancyHexCorners flatTop =
    let
        angleRad cornerNumber =
            if flatTop then
                degrees (60 * cornerNumber |> toFloat)

            else
                degrees (60 * cornerNumber - 30 |> toFloat)

        corner cornerNumber =
            if flatTop then
                ( (hexSize / 2) + hexSize * cos (angleRad cornerNumber)
                , (hexSize / 2) + hexSize * sin (angleRad cornerNumber)
                )

            else
                ( (hexSize / 2) + hexSize * cos (angleRad cornerNumber)
                , (hexSize / 2) + hexSize * sin (angleRad cornerNumber)
                )
    in
    HexCorners
        (corner 0)
        (corner 1)
        (corner 2)
        (corner 3)
        (corner 4)
        (corner 5)


renderHex : (( Point, tile ) -> Svg msg) -> ( Point, tile ) -> Svg msg
renderHex renderTile ( point, t ) =
    let
        ( x, y ) =
            pointToPixel False point
    in
    g [ Svg.Attributes.transform ("translate (" ++ String.fromFloat (x - hexSize / 2) ++ " " ++ String.fromFloat (y - hexSize / 2) ++ ") scale(0.99)") ]
        [ renderTile ( point, t ) ]


keyedViewHex : (( Point, tile ) -> Svg msg) -> ( Point, tile ) -> ( String, Svg msg )
keyedViewHex renderTile tile =
    ( Point.toString (Tuple.first tile)
    , Svg.Lazy.lazy (renderHex renderTile) tile
    )


edgeShadow : Svg msg
edgeShadow =
    Svg.radialGradient [ Svg.Attributes.id "edge-shadow" ]
        [ Svg.stop [ Svg.Attributes.offset "20%", Svg.Attributes.stopColor "black", Svg.Attributes.stopOpacity "0" ] []
        , Svg.stop [ Svg.Attributes.offset "99%", Svg.Attributes.stopColor "black" ] []
        ]


renderGrid : ( Float, Float ) -> RandomMap tile -> (( Point, tile ) -> Svg msg) -> Svg msg
renderGrid ( x, y ) map renderTile =
    svg
        [ Svg.Attributes.viewBox ([ -50, -50, 100, 100 ] |> List.map String.fromFloat |> List.intersperse " " |> String.concat)
        , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
        ]
        [ Svg.defs [] [ edgeShadow ]
        , Svg.Keyed.node "g" [ Svg.Attributes.style ("transform: translate(" ++ String.fromFloat -x ++ "px, " ++ String.fromFloat -y ++ "px);"), Svg.Attributes.id "root" ] (List.map (keyedViewHex renderTile) (Dict.toList map.grid))
        , Svg.rect
            [ Svg.Attributes.x "-50"
            , Svg.Attributes.y "-50"
            , Svg.Attributes.width "100"
            , Svg.Attributes.height "100"
            , Svg.Attributes.fill "url('#edge-shadow')"
            , Svg.Attributes.pointerEvents "none"
            ]
            []
        ]
