module HexEngine.RandomMap exposing
    ( RandomMap
    , empty
    , explore
    , exploreNeighbours
    , fieldOfVisionWithCost
    , insertReplaceHex
    , mapHexes
    , rayTraceWithCost
    , singleton
    , update
    )

import Dict exposing (Dict)
import HexEngine.Point as Point exposing (Point)
import Set
import Simplex exposing (PermutationTable)


{-| main hexmap, a dict with 3d points as keys and tile as values
-}
type alias RandomMap tile =
    { grid : Dict Point tile
    , config : MapGenerationConfig
    }


{-| Create a new empty map
-}
empty : Int -> RandomMap tile
empty seed =
    RandomMap (Dict.fromList [])
        (initMapGenConfig
            |> withSeed seed
            |> withScale 1
            |> withSteps 2
            |> withStepSize 4
            |> withPersistence 3
        )


{-| Create a new map with a given tile in the center
-}
singleton : Point -> tile -> Int -> RandomMap tile
singleton point tile seed =
    RandomMap (Dict.fromList [ ( point, tile ) ]) (initMapGenConfig |> withSeed seed)


mapHexes : (( Point, tile ) -> a) -> RandomMap tile -> List a
mapHexes f map =
    Dict.toList map.grid |> List.map f


update : Point -> (Maybe tile -> Maybe tile) -> RandomMap tile -> RandomMap tile
update point f map =
    { map | grid = Dict.update point f map.grid }


{-| explore a given point, will generate a new tile if none exists
-}
explore : (Float -> Maybe tile) -> Point -> RandomMap tile -> RandomMap tile
explore tileType point map =
    case Dict.get point map.grid of
        Just _ ->
            map

        Nothing ->
            case generateHex map.config tileType point of
                Just t ->
                    { map | grid = Dict.insert point t map.grid }

                Nothing ->
                    map


exploreNeighbours : (Float -> Maybe tile) -> Point -> RandomMap tile -> RandomMap tile
exploreNeighbours tileType point map =
    let
        points =
            Point.neighbors point |> Set.toList
    in
    insertHexes (List.filterMap (\p -> getHex tileType map p |> Maybe.andThen (\t -> Just ( p, t ))) points) map


{-| Get value of point, generate a new one if none exists
-}
getHex : (Float -> Maybe tile) -> RandomMap tile -> Point -> Maybe tile
getHex tileType map point =
    case Dict.get point map.grid of
        Just t ->
            Just t

        Nothing ->
            generateHex map.config tileType point


{-| Insert hex, do not replace if one exists at point
-}
insertHex : ( Point, tile ) -> RandomMap tile -> RandomMap tile
insertHex ( point, tile ) map =
    case Dict.get point map.grid of
        Just _ ->
            map

        Nothing ->
            { map | grid = Dict.insert point tile map.grid }


{-| Insert hex, replace on collision
-}
insertReplaceHex : ( Point, tile ) -> RandomMap tile -> RandomMap tile
insertReplaceHex ( point, tile ) map =
    { map | grid = Dict.insert point tile map.grid }


{-| Insert multiple hexes, does not replace
-}
insertHexes : List ( Point, tile ) -> RandomMap tile -> RandomMap tile
insertHexes hexes map =
    case hexes of
        [] ->
            map

        h :: hs ->
            insertHexes hs (insertHex h map)


{-| Get a line between two points, with cost for passing through tiles
The cost function defines how much it costs to pass throug a tile, a Nothing value means the tile can't be passed through
-}
rayTraceWithCost : Point -> Point -> Int -> (Float -> Maybe tile) -> (tile -> Maybe Int) -> RandomMap tile -> RandomMap tile
rayTraceWithCost from to cap tileType cost map =
    let
        -- Get a list of (Point, tile) representing a line between two points
        ray =
            Point.line from to

        visibleAcum point ( remaining, fs ) =
            let
                tile =
                    getHex tileType map point
            in
            if remaining > 0 then
                case tile of
                    Just t ->
                        case cost t of
                            Just c ->
                                ( remaining - c, ( point, t ) :: fs )

                            Nothing ->
                                ( 0, ( point, t ) :: fs )

                    Nothing ->
                        ( 0, fs )

            else
                ( 0, fs )

        ( _, visible ) =
            List.foldl visibleAcum ( cap, [] ) ray
    in
    insertHexes visible map


{-| Explores map in a radius, based on vision
-}
fieldOfVisionWithCost : Int -> Point -> (Float -> Maybe tile) -> (tile -> Maybe Int) -> RandomMap tile -> RandomMap tile
fieldOfVisionWithCost radius point tileType cost map =
    let
        ringPoints =
            Point.ring radius point
                |> Set.toList

        exploreLines : Point -> List Point -> RandomMap tile -> RandomMap tile
        exploreLines f l m =
            case l of
                [] ->
                    m

                p :: ps ->
                    exploreLines f ps (rayTraceWithCost f p radius tileType cost m)
    in
    exploreLines point ringPoints map



---- MAP GENERATOR ----


type alias MapGenerationConfig =
    { seed : Int
    , scale : Float
    , steps : Int
    , stepSize : Float
    , persistence : Float
    , permTable : PermutationTable
    }


initMapGenConfig : MapGenerationConfig
initMapGenConfig =
    MapGenerationConfig 42 1.0 3 2.5 6.0 (Simplex.permutationTableFromInt 42)


withSeed : Int -> MapGenerationConfig -> MapGenerationConfig
withSeed seed config =
    { config | seed = seed, permTable = Simplex.permutationTableFromInt seed }


withScale : Float -> MapGenerationConfig -> MapGenerationConfig
withScale scale config =
    { config | scale = scale }


withSteps : Int -> MapGenerationConfig -> MapGenerationConfig
withSteps steps config =
    { config | steps = steps }


withStepSize : Float -> MapGenerationConfig -> MapGenerationConfig
withStepSize stepSize config =
    { config | stepSize = stepSize }


withPersistence : Float -> MapGenerationConfig -> MapGenerationConfig
withPersistence persistence config =
    { config | persistence = persistence }


{-| Create 2D fractal noise
-}
noise : MapGenerationConfig -> Float -> Float -> Float
noise config x y =
    Simplex.fractal2d
        { scale = config.scale
        , steps = config.steps
        , stepSize = config.stepSize
        , persistence = config.persistence
        }
        config.permTable
        x
        y


generateHex : MapGenerationConfig -> (Float -> Maybe tile) -> Point -> Maybe tile
generateHex config tileType point =
    let
        ( ax, ay ) =
            Point.toAxial point

        pointValue =
            noise config (toFloat ax) (toFloat ay)
    in
    tileType pointValue
        |> Maybe.andThen (\t -> Just t)
