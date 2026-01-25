import System.Random

sampleRate = 1

frequencyOf :: Int -> Float
frequencyOf note = (2.0 ** (fromIntegral(note - 69) / 12.0)) * 440.0;

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
    let note = 57
    let f = frequencyOf note
    putStrLn ("generating " ++ show samples ++ " samples for note " ++ show note ++ " (" ++ show f ++ "Hz)")
    let w = wave f samples
    print(w)

