module HexEngine.Grid exposing
    ( HexGrid
    , empty
    , fieldOfVisionWithCost
    , filter
    , fromPoints
    , insert
    , movePoint
    , neighborValues
    , points
    , valueAt
    )

import Dict exposing (Dict)
import HexEngine.Point as Point exposing (Point, valid)
import Set exposing (Set)


{-| main hexmap, a dict with 3d points as keys and d as values
-}
type alias HexGrid tile =
    Dict Point tile


{-| Return a list of (Point, d) tuples that meet predicate
-}
filter : (( Point, tile ) -> Bool) -> HexGrid tile -> List ( Point, tile )
filter predicate grid =
    Dict.toList grid
        |> List.filter predicate


{-| Get a list of all points in a grid
-}
points : HexGrid tile -> List Point
points grid =
    Dict.keys grid


{-| Create a new empty grid
-}
empty : HexGrid tile
empty =
    Dict.fromList []


{-| Create a new grid from a list of points
-}
fromPoints : List Point -> tile -> HexGrid tile
fromPoints ps value =
    List.map (\p -> ( p, value )) ps |> Dict.fromList


{-| Insert data at point, replaces existing data if any
Returns unchanged grid if point is invalid
-}
insert : ( Point, tile ) -> HexGrid tile -> HexGrid tile
insert ( point, data ) grid =
    if valid point then
        Dict.insert point data grid

    else
        grid


{-| Get value at point if it exists
Similar to Dict.get, but the arguments are reversed for easier piping
-}
valueAt : HexGrid tile -> Point -> Maybe tile
valueAt grid point =
    Dict.get point grid


valueAtWithPoint : HexGrid tile -> Point -> Maybe ( Point, tile )
valueAtWithPoint grid point =
    Dict.get point grid |> Maybe.andThen (\t -> Just ( point, t ))


{-| move a tile on the grid, doesn't change grid if from is not found
-}
movePoint : Point -> Point -> HexGrid tile -> HexGrid tile
movePoint from to grid =
    let
        tile p =
            Dict.get p grid
    in
    case tile from of
        Just t ->
            Dict.remove from grid |> Dict.insert to t

        Nothing ->
            grid


{-| get a list of all ajacent values
-}
neighborValues : Point -> HexGrid tile -> List (Maybe tile)
neighborValues point grid =
    List.map (valueAt grid)
        (List.range 0 5
            |> List.map (Point.neighbor point)
        )


{-| Get a line between two points, with cost for passing through tiles
The cost function defines how much it costs to pass throug a tile, a Nothing value means the tile can't be passed through
-}
rayTraceWithCost : Point -> Point -> Int -> (tile -> Maybe Int) -> HexGrid tile -> Set Point
rayTraceWithCost from to cap cost grid =
    let
        ray =
            List.filterMap (valueAtWithPoint grid) (Point.line from to)

        visibleAcum ( point, tile ) ( remaining, fs ) =
            if remaining > 0 then
                case cost tile of
                    Just c ->
                        ( remaining - c, point :: fs )

                    Nothing ->
                        ( 0, point :: fs )

            else
                ( 0, fs )

        ( _, visible ) =
            List.foldl visibleAcum ( cap, [] ) ray
    in
    Set.union Set.empty (visible |> Set.fromList)


{-| Returns a set of visible points within radius, given a function that determines the cost of passing through a tile
-}
fieldOfVisionWithCost : Int -> Point -> (tile -> Maybe Int) -> HexGrid tile -> Set Point
fieldOfVisionWithCost radius point cost grid =
    let
        ringPoints =
            Point.ring radius point
                |> Set.toList
                |> List.map (\p -> rayTraceWithCost point p radius cost grid)
    in
    List.foldl Set.union Set.empty ringPoints
