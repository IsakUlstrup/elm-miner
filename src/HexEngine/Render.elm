module HexEngine.Render exposing
    ( RenderConfig
    , cornersToString
    , fancyHexCorners
    , initRenderConfig
    , renderGrid
    , withCameraPosition
    , withFlatTop
    , withHexFocus
    , withZoom
    )

import HexEngine.Point as Point exposing (Point)
import HexEngine.RandomMap exposing (RandomMap, mapHexes)
import Svg exposing (Svg, g, svg)
import Svg.Attributes
import Svg.Keyed
import Svg.Lazy



---- CONFIG BUILDER ----


type alias RenderConfig =
    { cameraX : Float
    , cameraY : Float
    , zoom : Float
    , flatTop : Bool
    }


initRenderConfig : RenderConfig
initRenderConfig =
    RenderConfig 0 0 1 False


withCameraPosition : ( Float, Float ) -> RenderConfig -> RenderConfig
withCameraPosition ( x, y ) config =
    { config | cameraX = x, cameraY = y }


{-| move camera to focus on point
-}
withHexFocus : Point -> RenderConfig -> RenderConfig
withHexFocus point config =
    let
        pos =
            point |> pointToPixel config
    in
    config |> withCameraPosition pos


withZoom : Float -> RenderConfig -> RenderConfig
withZoom zoom config =
    { config | zoom = zoom }


withFlatTop : Bool -> RenderConfig -> RenderConfig
withFlatTop flat config =
    { config | flatTop = flat }



---- GENERAL STUFF ----


{-| Hex size constant
-}
hexSize : Float
hexSize =
    5


{-| Get the center of a given point in screen coordinates
-}
pointToPixel : RenderConfig -> Point -> ( Float, Float )
pointToPixel config point =
    let
        ( q, r ) =
            Point.toAxial point
    in
    if config.flatTop then
        ( hexSize * (3 / 2 * toFloat q)
        , hexSize * (sqrt 3 / 2 * toFloat q + sqrt 3 * toFloat r)
        )

    else
        ( hexSize * (sqrt 3 * toFloat q + sqrt 3 / 2 * toFloat r)
        , hexSize * (3 / 2 * toFloat r)
        )


cornersToString : List ( Float, Float ) -> String
cornersToString points =
    let
        tupleToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    List.map tupleToString points |> List.intersperse " " |> String.concat


{-| Calculate hex corners in screen coordinates
-}
fancyHexCorners : RenderConfig -> List ( Float, Float )
fancyHexCorners config =
    let
        angleRad cornerNumber =
            if config.flatTop then
                degrees (60 * cornerNumber |> toFloat)

            else
                degrees (60 * cornerNumber - 30 |> toFloat)

        corner cornerNumber =
            if config.flatTop then
                ( (hexSize / 2) + hexSize * cos (angleRad cornerNumber)
                , (hexSize / 2) + hexSize * sin (angleRad cornerNumber)
                )

            else
                ( (hexSize / 2) + hexSize * cos (angleRad cornerNumber)
                , (hexSize / 2) + hexSize * sin (angleRad cornerNumber)
                )
    in
    [ corner 0
    , corner 1
    , corner 2
    , corner 3
    , corner 4
    , corner 5
    ]


renderHex : RenderConfig -> (( Point, tile ) -> Svg msg) -> ( Point, tile ) -> Svg msg
renderHex config renderTile ( point, t ) =
    let
        ( x, y ) =
            pointToPixel config point
    in
    g [ Svg.Attributes.transform ("translate (" ++ String.fromFloat (x - hexSize / 2) ++ " " ++ String.fromFloat (y - hexSize / 2) ++ ") scale(0.99)") ]
        [ renderTile ( point, t ) ]


keyedViewHex : RenderConfig -> (( Point, tile ) -> Svg msg) -> ( Point, tile ) -> ( String, Svg msg )
keyedViewHex config renderTile tile =
    ( Point.toString (Tuple.first tile)
    , Svg.Lazy.lazy (renderHex config renderTile) tile
    )


edgeShadow : Svg msg
edgeShadow =
    Svg.radialGradient [ Svg.Attributes.id "edge-shadow" ]
        [ Svg.stop [ Svg.Attributes.offset "0%", Svg.Attributes.stopColor "black", Svg.Attributes.stopOpacity "0" ] []
        , Svg.stop [ Svg.Attributes.offset "100%", Svg.Attributes.stopColor "black", Svg.Attributes.stopOpacity "0.6" ] []
        ]


renderGrid : RenderConfig -> RandomMap tile -> (( Point, tile ) -> Svg msg) -> Svg msg
renderGrid config map renderTile =
    svg
        [ Svg.Attributes.viewBox ([ -50, -50, 100, 100 ] |> List.map String.fromFloat |> List.intersperse " " |> String.concat)
        , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
        ]
        [ Svg.defs [] [ edgeShadow ]
        , Svg.Keyed.node "g"
            [ Svg.Attributes.style
                ("transform: translate("
                    ++ String.fromFloat -(config.cameraX * config.zoom)
                    ++ "px, "
                    ++ String.fromFloat -(config.cameraY * config.zoom)
                    ++ "px) scale("
                    ++ String.fromFloat config.zoom
                    ++ ");"
                )
            , Svg.Attributes.id "root"
            ]
            (mapHexes (keyedViewHex config renderTile) map)
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
