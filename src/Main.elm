module Main exposing (..)

-- import HexEngine.Grid as Grid exposing (HexGrid)
-- import HexEngine.GridGenerator as GridGen exposing (MapGenerationConfig, initMapGenConfig)

import Browser
import Dict
import HexEngine.Point as Point exposing (Point)
import HexEngine.RandomMap exposing (RandomMap, explore, exploreNeighbours, fieldOfVisionWithCost, insertReplaceHex, rayTraceWithCost, singleton)
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
    if val < -0.2 then
        Just Ground

    else if val < 0.7 then
        Just Rock

    else
        Just Ore


visionCost : Tile -> Maybe Int
visionCost tile =
    case tile of
        Ground ->
            Just 1

        Rock ->
            Nothing

        Ore ->
            Nothing

        CampFire ->
            Just 2


vision : Point -> RandomMap Tile -> RandomMap Tile
vision point =
    fieldOfVisionWithCost 5 point tileType visionCost



---- MODEL ----


type Tile
    = Ground
    | Rock
    | Ore
    | CampFire


type alias Model =
    { map : RandomMap Tile
    , stamina : Float
    , lastHex : Point
    }


initMap : RandomMap Tile
initMap =
    singleton CampFire 1 |> exploreNeighbours tileType ( 0, 0, 0 )


init : ( Model, Cmd Msg )
init =
    ( { map = initMap
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExploreTile point ->
            ( { model | map = vision point model.map, lastHex = point }, Cmd.none )

        DestroyTile point ->
            if model.stamina > 0 then
                ( { model
                    | map =
                        model.map
                            |> insertReplaceHex ( point, Ground )
                            |> vision point
                    , stamina = model.stamina - 1
                    , lastHex = point
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Rest point ->
            ( { model | map = initMap, stamina = 10, lastHex = point }, Cmd.none )



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
