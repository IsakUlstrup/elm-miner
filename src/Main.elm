module Main exposing (..)

-- import HexEngine.Grid as Grid exposing (HexGrid)
-- import HexEngine.GridGenerator as GridGen exposing (MapGenerationConfig, initMapGenConfig)

import Browser
import HexEngine.Point exposing (Point)
import HexEngine.RandomMap exposing (RandomMap, exploreNeighbours, fieldOfVisionWithCost, insertReplaceHex, singleton)
import HexEngine.Render exposing (cornersToString, fancyHexCorners, pointToPixel, renderGrid)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events
import Player exposing (Player)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events



---- MAP ----


tileType : Float -> Maybe Tile
tileType val =
    if val < -0.5 then
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


vision : Int -> Point -> RandomMap Tile -> RandomMap Tile
vision range point =
    fieldOfVisionWithCost range point tileType visionCost



---- MODEL ----


type Tile
    = Ground
    | Rock
    | Ore
    | CampFire


type alias Model =
    { map : RandomMap Tile
    , player : Player
    , lastHex : Point
    }


initMap : RandomMap Tile
initMap =
    singleton CampFire 1 |> exploreNeighbours tileType ( 0, 0, 0 )


init : ( Model, Cmd Msg )
init =
    ( { map = initMap
      , player = Player.new
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
                            |> insertReplaceHex ( point, Ground )
                            |> vision model.player.perception point
                    , player = Player.useStamina 1 model.player
                    , lastHex = point
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Rest point ->
            ( { model
                | map = initMap
                , player = Player.rest model.player
                , lastHex = point
              }
            , Cmd.none
            )

        DrinkBeer ->
            ( { model | player = Player.drinkBeer model.player }, Cmd.none )



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
        [ div [ id "game-ui" ] [ text ("Stamina: " ++ Player.staminaToString model.player) ]
        , renderGrid (model.lastHex |> pointToPixel False) model.map renderTile
        , div [ id "game-skills" ]
            [ button [ Html.Events.onClick DrinkBeer, Html.Attributes.class "skill" ]
                [ text
                    ("🍺"
                        ++ (Tuple.first model.player.beer |> String.fromInt)
                    )
                ]
            , button [ Html.Events.onClick DrinkBeer, Html.Attributes.class "skill" ]
                [ text
                    ("💣"
                        ++ (Tuple.first model.player.beer |> String.fromInt)
                    )
                ]
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
