import System.Random

sampleRate = 44100

sineOscillator :: Float -> Int -> Float
sineOscillator f i = sin (f * 2 * pi * fromIntegral (i) / 44100)

wave :: Float -> Int -> [Float]
wave f n = map (\x -> sineOscillator f x) [0..n]


randomLength :: Int -> Int -> IO Int
randomLength min maxRange = do
  r1 <- getStdGen
  let (x, r2) = randomR (min * sampleRate, maxRange * sampleRate) r1
  setStdGen r2
  return x

main = do
    gen <- randomIO :: IO Float
    print $ (\x -> (sampleRate * 6) + floor (fromIntegral (sampleRate * 14) * x)) gen
    let w = wave 440.0 15
    print(w)
    g <- getStdGen
    print . take 1 $ (randoms g :: [Float])
