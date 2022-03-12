module Main exposing (..)

import Browser
import Dict
import HexEngine.Grid as Grid exposing (HexGrid)
import HexEngine.GridGenerator as GridGen exposing (MapGenerationConfig, initMapGenConfig)
import HexEngine.Point as Point exposing (Point)
import HexEngine.Render exposing (cornersToString, fancyHexCorners, pointToPixel, renderGrid)
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Set
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events



---- MAP ----


tileType : Float -> Maybe Tile
tileType val =
    if val < -0.5 then
        Just Ground

    else if val < 0.5 then
        Just Rock

    else
        Just Ore



---- MODEL ----


type Tile
    = Ground
    | Rock
    | Ore
    | CampFire


type alias Model =
    { map : HexGrid Tile
    , mapGenConfig : GridGen.MapGenerationConfig
    , stamina : Float
    , lastHex : Point
    }


initMap : MapGenerationConfig -> HexGrid Tile
initMap cfg =
    exploreNeighbours ( 0, 0, 0 ) cfg |> Grid.insert ( ( 0, 0, 0 ), CampFire )


init : ( Model, Cmd Msg )
init =
    let
        mapGenCfg =
            initMapGenConfig |> GridGen.withSeed 13
    in
    ( { map = initMap mapGenCfg
      , mapGenConfig = mapGenCfg
      , stamina = 10
      , lastHex = ( 0, 0, 0 )
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ExploreTile Point
    | DestroyTile Point
    | Rest Point


exploreNeighbours : Point -> MapGenerationConfig -> HexGrid Tile
exploreNeighbours point mapGenCfg =
    Point.neighbors point
        |> Set.toList
        |> List.filterMap (GridGen.randomHex mapGenCfg tileType)
        |> Dict.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExploreTile point ->
            ( { model | map = Dict.union model.map (exploreNeighbours point model.mapGenConfig), lastHex = point }, Cmd.none )

        DestroyTile point ->
            if model.stamina > 0 then
                ( { model
                    | map = Dict.union model.map (exploreNeighbours point model.mapGenConfig) |> Grid.insert ( point, Ground )
                    , stamina = model.stamina - 1
                    , lastHex = point
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Rest point ->
            ( { model | map = initMap model.mapGenConfig, stamina = 10, lastHex = point }, Cmd.none )



---- VIEW ----


renderTile : ( Point, Tile ) -> Svg Msg
renderTile ( point, tile ) =
    case tile of
        Ground ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners False |> cornersToString)
                , Svg.Attributes.class "ground"
                , Svg.Attributes.class "hex"
                , Svg.Events.onClick (ExploreTile point)
                ]
                []

        Rock ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners False |> cornersToString)
                , Svg.Attributes.class "rock"
                , Svg.Attributes.class "hex"
                , Svg.Events.onClick (DestroyTile point)
                ]
                []

        Ore ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners False |> cornersToString)
                , Svg.Attributes.class "ore"
                , Svg.Attributes.class "hex"
                , Svg.Events.onClick (DestroyTile point)
                ]
                []

        CampFire ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners False |> cornersToString)
                , Svg.Attributes.class "campFire"
                , Svg.Attributes.class "hex"
                , Svg.Events.onClick (Rest point)
                ]
                []


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ div [ id "game-ui" ] [ text ("Stamina: " ++ String.fromFloat model.stamina) ]
        , renderGrid (model.lastHex |> pointToPixel False) model.map renderTile
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
