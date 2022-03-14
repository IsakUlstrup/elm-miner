module HexEngine.GridGenerator exposing
    ( MapGenerationConfig
    , initMapGenConfig
    , randomHex
    , withPersistence
    , withScale
    , withSeed
    , withStepSize
    , withSteps
    )

import Dict
import HexEngine.Grid exposing (HexGrid)
import HexEngine.Point as Point exposing (Point)
import Set
import Simplex exposing (PermutationTable)


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


randomHex : MapGenerationConfig -> (Float -> Maybe tile) -> Point -> Maybe ( Point, tile )
randomHex config tileType point =
    let
        ( ax, ay ) =
            Point.toAxial point

        pointValue =
            noise config (toFloat ax) (toFloat ay)
    in
    tileType pointValue
        |> Maybe.andThen (\t -> Just ( point, t ))
