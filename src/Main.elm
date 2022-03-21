module Main exposing (..)

import Browser
import HexEngine.Point exposing (Point)
import HexEngine.RandomMap as Map exposing (RandomMap, exploreNeighbours, fieldOfVisionWithCost, setMapGenConfig, singleton)
import HexEngine.Render exposing (RenderConfig, cornersToString, fancyHexCorners, initRenderConfig, renderGrid, withHexFocus, withZoom)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events
import Player exposing (Player)
import Random
import Range
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Tile exposing (Biome(..), Tile(..), campFire, ground, ore, rock)



---- MAP ----


randomGround : Biome -> Random.Generator Tile
randomGround biome =
    Random.weighted
        ( 100, ground biome )
        [ ( 2, campFire )
        ]


randomRock : Biome -> Random.Generator Tile
randomRock biome =
    Random.weighted
        ( 100, rock 10 biome )
        [ ( 10, ore 10 )
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
            if v < 0.42 || v > 0.58 then
                Random.step (randomRock b) (Random.initialSeed (v * 1000 |> round)) |> Tuple.first

            else
                Random.step (randomGround b) (Random.initialSeed (v * 1000 |> round)) |> Tuple.first
    in
    Just (biome val)


visionCost : Tile -> Maybe Int
visionCost tile =
    case tile of
        Ground _ ->
            Just 1

        Rock _ _ ->
            Nothing

        Ore _ ->
            Nothing

        CampFire ->
            Just 2


vision : Int -> Point -> RandomMap Tile -> RandomMap Tile
vision range point =
    fieldOfVisionWithCost range point tileType visionCost



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
        |> exploreNeighbours tileType point


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
    | DestroyTile Point
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

        DestroyTile point ->
            if Player.hasStamina model.player then
                ( { model
                    | map =
                        model.map
                            |> Map.update point (Tile.damageTile model.player.damage)
                            |> vision model.player.perception point
                    , player = Player.useStamina 1 model.player
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
    let
        class b =
            case b of
                Cold ->
                    "hex cold"

                Neutral ->
                    "hex neutral"

                Fire ->
                    "hex fire"
    in
    case tile of
        Ground biome ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners defaultRenderConfig |> cornersToString)
                , Svg.Attributes.class (class biome)
                , Svg.Attributes.class "ground"
                , Svg.Events.onClick (ExploreTile point)
                ]
                []

        Rock _ biome ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners defaultRenderConfig |> cornersToString)
                , Svg.Attributes.class "rock"
                , Svg.Attributes.class (class biome)
                , Svg.Events.onClick (DestroyTile point)
                ]
                []

        Ore _ ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners defaultRenderConfig |> cornersToString)
                , Svg.Attributes.class "ore"
                , Svg.Attributes.class "hex"
                , Svg.Events.onClick (DestroyTile point)
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
        [ div [ id "game-ui" ] [ text ("Stamina: " ++ Player.staminaToString model.player) ]
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
