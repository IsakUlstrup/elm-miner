module HexEngine.Point exposing
    ( Point
    , add
    , fieldOfVision
    , fieldOfVisionWithCost
    , fromAxial
    , fromFloat
    , getX
    , getY
    , getZ
    , lerp
    , line
    , neighbor
    , neighbors
    , rayTraceWithCost
    , ring
    , scale
    , subtract
    , toAxial
    , toInt
    , toString
    , valid
    )

import Set exposing (Set)


{-| A 3d point
-}
type alias Point =
    ( Int, Int, Int )


{-| Create a new point from float values, round to nearest valid point
-}
fromFloat : ( Float, Float, Float ) -> Point
fromFloat ( x, y, z ) =
    let
        -- rounded point
        ( rx, ry, rz ) =
            ( round x, round y, round z )

        -- diierence between input point and rounded point
        ( dx, dy, dz ) =
            ( abs (toFloat rx - x), abs (toFloat ry - y), abs (toFloat rz - z) )

        -- final adjusted point
        ( fx, fy, fz ) =
            if dx > dy && dx > dz then
                ( -ry - rz, ry, rz )

            else if dy > dz then
                ( rx, -rx - rz, rz )

            else
                ( rx, ry, -rx - ry )
    in
    ( fx, fy, fz )


{-| Check if a point is valid
A valid point is one where x + y + z == 0
-}
valid : Point -> Bool
valid ( x, y, z ) =
    x + y + z == 0


{-| Convert cube point to axial point
Note: returns (0, 0) if point is invalid
-}
toAxial : Point -> ( Int, Int )
toAxial ( x, y, z ) =
    if valid ( x, y, z ) then
        ( x, z )

    else
        ( 0, 0 )


{-| Convert point to a hopefully unique number, useful for deterministic RNG
-}
toInt : Point -> Int
toInt ( x, y, z ) =
    (x * -10) + y + (z * 10)


fromAxial : ( Int, Int ) -> Point
fromAxial ( q, r ) =
    ( q, r, -q - r )


{-| get x component of a point
-}
getX : Point -> Int
getX ( x, _, _ ) =
    x


{-| get y component of a point
-}
getY : Point -> Int
getY ( _, y, _ ) =
    y


{-| get z component of a point
-}
getZ : Point -> Int
getZ ( _, _, z ) =
    z


{-| Add two points together
-}
add : Point -> Point -> Point
add ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 + x2, y1 + y2, z1 + z2 )


{-| Subtract point from point
-}
subtract : Point -> Point -> Point
subtract ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 - x2, y1 - y2, z1 - z2 )


{-| Scale point
-}
scale : Float -> Point -> Point
scale i ( x1, y1, z1 ) =
    ( toFloat x1 * i, toFloat y1 * i, toFloat z1 * i ) |> fromFloat


{-| Get direction given hex side
-}
direction : Int -> Point
direction dir =
    case dir of
        0 ->
            ( 1, -1, 0 )

        1 ->
            ( 1, 0, -1 )

        2 ->
            ( 0, 1, -1 )

        3 ->
            ( -1, 1, 0 )

        4 ->
            ( -1, 0, 1 )

        _ ->
            ( 0, -1, 1 )


{-| Point linear interpolation
-}
lerp : Float -> Point -> Point -> Point
lerp t ( x1, y1, z1 ) ( x2, y2, z2 ) =
    let
        lrp : Float -> Float -> Float -> Float
        lrp a b tt =
            a + (b - a) * tt
    in
    -- add p1 (subtract p2 p1) |> multiply t
    ( lrp (x1 |> toFloat) (x2 |> toFloat) t
    , lrp (y1 |> toFloat) (toFloat y2) t
    , lrp (toFloat z1) (toFloat z2) t
    )
        |> fromFloat


{-| Convert Point to string with format "(x,y,z)"
-}
toString : Point -> String
toString ( x, y, z ) =
    [ "("
    , String.fromInt x
    , ","
    , String.fromInt y
    , ","
    , String.fromInt z
    , ")"
    ]
        |> String.concat


{-| Get the neighbor at direction dir of a given point
-}
neighbor : Point -> Int -> Point
neighbor point dir =
    add point (direction dir)


{-| Get all six neighbors
-}
neighbors : Point -> Set Point
neighbors p =
    [ neighbor p 0
    , neighbor p 1
    , neighbor p 2
    , neighbor p 3
    , neighbor p 4
    , neighbor p 5
    ]
        |> Set.fromList


{-| get distance between two points
-}
distance : Point -> Point -> Int
distance p1 p2 =
    distanceFloat p1 p2 |> round


distanceFloat : Point -> Point -> Float
distanceFloat ( x1, y1, z1 ) ( x2, y2, z2 ) =
    toFloat (abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)) / 2


{-| Return list of points that form a line between two points
-}
line : Point -> Point -> List Point
line from to =
    let
        dist =
            distance from to

        range =
            List.map toFloat (List.range 0 dist)
    in
    if from == to then
        []

    else
        List.map
            (\i -> lerp (1.0 / toFloat dist * i) from to)
            range


{-| Get a line between two points, but stop at any obstacles
-}
rayTrace : Point -> Point -> Set Point -> Set Point
rayTrace from to obstacles =
    let
        ray =
            line from to

        visibleAcum point ( hitWall, fs ) =
            -- obstacle hit, return visible
            if hitWall then
                ( True, fs )
                -- obstacle hit, add obstacle to visible

            else if Set.member point obstacles then
                ( True, point :: fs )
                -- obstacle not hit, add point to visible and continue

            else
                ( False, point :: fs )

        ( _, visible ) =
            List.foldl visibleAcum ( False, [] ) ray
    in
    Set.union Set.empty (visible |> Set.fromList)


{-| Get a line between two points, with cost for passing through tiles
The cost function defines how much it costs to pass throug a tile, a Nothing value means the tile can't be passed through
-}
rayTraceWithCost : Point -> Point -> Int -> (Point -> Maybe Int) -> Set Point
rayTraceWithCost from to cap cost =
    let
        ray =
            line from to

        visibleAcum point ( remaining, fs ) =
            if remaining > 0 then
                case cost point of
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


{-| Returns a set of unobstructed points within radius
-}
fieldOfVision : Int -> Point -> Set Point -> Set Point
fieldOfVision radius point obstacles =
    let
        ringPoints =
            ring radius point
                |> Set.toList
                |> List.map (\p -> rayTrace point p obstacles)
    in
    List.foldl Set.union Set.empty ringPoints


{-| Returns a set of visible points within radius, given a function that determines the cost of passing through a tile
-}
fieldOfVisionWithCost : Int -> Point -> (Point -> Maybe Int) -> Set Point
fieldOfVisionWithCost radius point cost =
    let
        ringPoints =
            ring radius point
                |> Set.toList
                |> List.map (\p -> rayTraceWithCost point p radius cost)
    in
    List.foldl Set.union Set.empty ringPoints


{-| Returns a ring around given point with given radius
This is more of a hex for now. Will fix later
-}
ring : Int -> Point -> Set Point
ring radius center =
    let
        getDirection s =
            case s of
                0 ->
                    4

                1 ->
                    5

                2 ->
                    0

                3 ->
                    1

                4 ->
                    2

                _ ->
                    3

        start s =
            add center (scale (toFloat radius) (direction (getDirection s)))

        side s =
            List.map (\i -> add (start s) (scale (toFloat i) (direction s))) (List.range 0 radius)
    in
    List.concatMap side (List.range 0 6) |> Set.fromList
