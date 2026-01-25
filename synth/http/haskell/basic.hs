import System.Random

sampleRate = 1

sineOscillator :: Float -> Int -> Float
sineOscillator f i = sin (f * 2 * pi * fromIntegral (i) / 44100)

wave :: Float -> Int -> [Float]
wave f n = map (\x -> sineOscillator f x) [0..n]

valueOf :: Int -> Int
valueOf x = x


main = do
    gen <- randomIO :: IO Float
    v <- randomRIO (sampleRate * 6, sampleRate * 14)
    let samples = valueOf v
    putStrLn ("generating " ++ show samples ++ " samples")
    let w = wave 440.0 samples
    print(w)

