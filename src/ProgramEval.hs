module ProgramEval where

numbersStartingAt :: Num t => t -> [t]
numbersStartingAt n =
            n: numbersStartingAt (n+1)

radsToDegrees::Float -> Int
radsToDegrees radians =
    let degrees = cycle [0..359]
        converted = truncate $ (radians * 360) / (2 * pi)
    in degrees !! converted
