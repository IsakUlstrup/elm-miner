module Main exposing (..)

import Browser
import HexEngine.Point exposing (Point)
import HexEngine.RandomMap as Map exposing (RandomMap, exploreNeighbours, fieldOfVisionWithCost, singleton)
import HexEngine.Render exposing (cornersToString, fancyHexCorners, pointToPixel, renderGrid)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events
import Player exposing (Player)
import Range
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Tile exposing (Tile(..))



---- MAP ----


tileType : Float -> Maybe Tile
tileType val =
    if val < -0.5 then
        Just Tile.ground

    else if val < -0.494 then
        Just Tile.campFire

    else if val < 0.7 then
        Just (Tile.rock 10)

    else
        Just (Tile.ore 10)


visionCost : Tile -> Maybe Int
visionCost tile =
    case tile of
        Ground ->
            Just 1

        Rock _ ->
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
    singleton point CampFire 1 |> exploreNeighbours tileType point


init : ( Model, Cmd Msg )
init =
    ( { map = initMap ( 0, 0, 0 )
      , player = Player.new 5
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
                            |> Map.update point (Tile.damage 2)
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

        Rock _ ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners False |> cornersToString)
                , Svg.Attributes.class "rock"
                , Svg.Attributes.class "hex"
                , Svg.Events.onClick (DestroyTile point)
                ]
                []

        Ore _ ->
            Svg.polygon
                [ Svg.Attributes.points (fancyHexCorners False |> cornersToString)
                , Svg.Attributes.class "ore"
                , Svg.Attributes.class "hex"
                , Svg.Events.onClick (DestroyTile point)
                ]
                []

        CampFire ->
            Svg.g []
                [ Svg.polygon
                    [ Svg.Attributes.points (fancyHexCorners False |> cornersToString)
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
        , renderGrid (model.lastHex |> pointToPixel False) model.map renderTile
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
