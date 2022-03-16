module Range exposing
    ( Range
    , add
    , getValue
    , isEmpty
    , isFull
    , newRange
    , setHigh
    , setLow
    , setValue
    , setValueWrap
    , subtract
    , viewHelperFloat
    , viewHelperInt
    )

{-| A range represents a number limited to between two numbers
-}


type Range number
    = Range
        { value : number
        , low : number
        , high : number
        }


{-| Create new Range

If high is less than low, high will be set to low

-}
newRange : number -> number -> number -> Range number
newRange low high val =
    let
        clampedHigh =
            if high < low then
                low

            else
                high
    in
    Range
        { value = clamp low clampedHigh val
        , low = low
        , high = clampedHigh
        }


{-| Set Range value
-}
setValue : number -> Range number -> Range number
setValue val (Range range) =
    Range { range | value = clamp range.low range.high val }


{-| Like clamp, but will "wrap" value around from low if above high.

Example: clampWrap 0 100 110 returns 10

-}
clampWrap : number -> number -> number -> number
clampWrap low high number =
    if number > high then
        clampWrap low high (number - high)

    else if number < low then
        clampWrap low high (number + high)

    else
        number


{-|

    Set Range value, but "wrap" value around if it's above cap

    Example: newRange 0 100 |> setValueWrap 110 = Range 10 100

-}
setValueWrap : number -> Range number -> Range number
setValueWrap val (Range range) =
    Range { range | value = clampWrap range.low range.high val }


{-| Get Range value
-}
getValue : Range number -> number
getValue (Range range) =
    range.value


{-| Add amount to Range value
-}
add : number -> Range number -> Range number
add amount ((Range range) as m) =
    setValue (range.value + clamp range.low range.high amount) m


{-| Subtract amount from Range value
-}
subtract : number -> Range number -> Range number
subtract amount ((Range range) as m) =
    setValue (range.value - max 0 amount) m


{-| Set Range value to low
-}
setLow : Range number -> Range number
setLow (Range range) =
    Range { range | value = 0 }


{-| Set Range value to high
-}
setHigh : Range number -> Range number
setHigh (Range range) =
    Range { range | value = range.high }


{-| Is Range low?
-}
isEmpty : Range number -> Bool
isEmpty (Range range) =
    range.value <= range.low


{-| Is Range high?
-}
isFull : Range number -> Bool
isFull (Range range) =
    range.value >= range.high


{-| Get some values that makes rendering a float Range easier
-}
viewHelperFloat : Range Float -> { min : String, max : String, value : String }
viewHelperFloat (Range range) =
    { min = "0", max = range.high |> String.fromFloat, value = range.value |> String.fromFloat }


{-| Get some values that makes rendering a float Range easier
-}
viewHelperInt : Range Int -> { min : String, max : String, value : String }
viewHelperInt (Range range) =
    { min = "0", max = range.value |> String.fromInt, value = range.value |> String.fromInt }
