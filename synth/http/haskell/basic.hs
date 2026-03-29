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


times_n :: Int -> Int -> Int
times_n n x = x * n



main = do
    gen <- randomIO :: IO Float
    v <- randomRIO (sampleRate * 6, sampleRate * 14)
    let samples = valueOf v
    let note = 57
    let f = frequencyOf note
    let str = "generating " ++ show samples ++ " samples for note " ++ show note ++ " (" ++ show f ++ "Hz)"
    putStrLn (str)
    let w = wave f samples
    print(w)
    
    
    let v = [0..4]
    print (map ((7*).(4*)) v)
    
    let f = [times_n 2, times_n 3]
        ff = foldl (.) (1*) f
    
    print(ff 1)

