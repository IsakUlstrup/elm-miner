module Main exposing (..)

import Browser
import Color exposing (initColor, toCssString, withHue, withLightness)
import HexEngine.Point as Point exposing (Point)
import HexEngine.RandomMap as Map exposing (RandomMap, exploreNeighbours, fieldOfVisionWithCost, setMapGenConfig, singleton)
import HexEngine.Render exposing (RenderConfig, cornersToString, fancyHexCorners, initRenderConfig, renderGrid, withHexFocus, withZoom)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events
import Item exposing (Item, ironOre)
import MapGen exposing (rainbow, tileType)
import Player exposing (Player)
import Random
import Range
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Tile exposing (Biome(..), Tile(..))



---- MAP ----


randomOreLoot : Random.Generator (List Item)
randomOreLoot =
    Random.weighted
        ( 100, List.repeat 1 ironOre )
        [ ( 10, List.repeat 3 ironOre )
        ]


visionCost : Tile -> Maybe Int
visionCost tile =
    case tile of
        Ground _ ->
            Just 1

        Rock _ ->
            Nothing

        Ore _ ->
            Nothing

        CampFire ->
            Just 2


vision : Int -> Point -> RandomMap Tile -> RandomMap Tile
vision range point =
    fieldOfVisionWithCost range point rainbow visionCost



---- MODEL ----


type alias Model =
    { map : RandomMap Tile
    , player : Player
    , lastHex : Point
    }


initMap : Point -> RandomMap Tile
initMap point =
    singleton point CampFire 1
        |> setMapGenConfig (Map.withScale 1 >> Map.withStepSize 6 >> Map.withPersistence 5)
        |> exploreNeighbours rainbow point


init : ( Model, Cmd Msg )
init =
    ( { map = initMap ( 0, 0, 0 )
      , player = Player.new 25 10
      , lastHex = ( 0, 0, 0 )
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ExploreTile Point
    | DestroyTile Point Tile
    | Rest Point
    | DrinkBeer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExploreTile point ->
            ( { model
                | map = vision model.player.perception point model.map
                , lastHex = point
              }
            , Cmd.none
            )

        DestroyTile point tile ->
            if Player.hasStamina model.player then
                let
                    loot =
                        case tile of
                            Ore _ ->
                                Random.step randomOreLoot (Random.initialSeed (Point.toInt point)) |> Tuple.first

                            _ ->
                                []
                in
                ( { model
                    | map =
                        model.map
                            |> Map.update point Tile.damageTile
                            |> vision model.player.perception point
                    , player = Player.useStamina 1 model.player |> Player.lootItems loot
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Rest point ->
            ( { model
                | map = initMap point
                , player = Player.rest model.player
                , lastHex = point
              }
            , Cmd.none
            )

        DrinkBeer ->
            ( { model | player = Player.drinkBeer model.player }, Cmd.none )



---- VIEW ----


defaultRenderConfig : RenderConfig
defaultRenderConfig =
    initRenderConfig |> withZoom 1


renderTile : ( Point, Tile ) -> Svg Msg
renderTile ( point, tile ) =
    case tile of
        Ground biome ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners defaultRenderConfig |> cornersToString)

                -- , Svg.Attributes.class (class biome)
                , Svg.Attributes.fill (biome |> withLightness 20 |> toCssString)
                , Svg.Attributes.class "ground"
                , Svg.Events.onClick (ExploreTile point)
                ]
                []

        Rock biome ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners defaultRenderConfig |> cornersToString)
                , Svg.Attributes.class "rock"
                , Svg.Attributes.fill (biome |> toCssString)

                -- , Svg.Attributes.class (class biome)
                , Svg.Events.onClick (DestroyTile point tile)
                ]
                []

        Ore biome ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners defaultRenderConfig |> cornersToString)
                , Svg.Attributes.class "ore"
                , Svg.Attributes.fill (biome |> toCssString)

                -- , Svg.Attributes.class (class biome)
                , Svg.Events.onClick (DestroyTile point tile)
                ]
                []

        CampFire ->
            Svg.g []
                [ Svg.polygon
                    [ Svg.Attributes.points (fancyHexCorners defaultRenderConfig |> cornersToString)
                    , Svg.Attributes.class "campFire"
                    , Svg.Attributes.class "hex"
                    , Svg.Events.onClick (Rest point)
                    ]
                    []
                , Svg.text_
                    [ Svg.Attributes.fontSize "0.5rem"
                    , Svg.Attributes.textAnchor "middle"
                    , Svg.Attributes.x "2.5"
                    , Svg.Attributes.y "5"
                    , Svg.Attributes.pointerEvents "none"
                    ]
                    [ text "🏕️" ]
                ]


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ div [ id "game-ui" ]
            [ text ("Stamina: " ++ Player.staminaToString model.player)
            , Html.br [] []
            , text ("Inventory: " ++ String.fromInt (List.length model.player.inventory))
            ]
        , renderGrid (defaultRenderConfig |> withHexFocus model.lastHex) model.map renderTile
        , div [ id "game-skills" ]
            [ button [ Html.Events.onClick DrinkBeer, Html.Attributes.class "skill" ]
                [ text ("🍺" ++ (Range.viewHelperInt model.player.beer).value) ]
            ]
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
