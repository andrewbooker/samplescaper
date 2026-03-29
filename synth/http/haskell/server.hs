{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (queryString, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, queryToQueryText)
import Network.HTTP.Types.Header (hContentType, hContentLength)
import Data.Word (Word32)
import Data.Function
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import GHC.Float (castFloatToWord32)
import Data.Binary.Put (runPut, putWord32le)
import System.Random (randomRIO)
import System.Environment (getArgs)


sampleRate = 44100

quotientOf :: Int -> Int -> Float
quotientOf a b = (fromIntegral a) / (fromIntegral b)

frequencyOf :: Int -> Float
frequencyOf note = (2.0 ** (fromIntegral (note - 69) / 12.0)) * 440.0;


amplitude :: Float -> Float -> Float
amplitude s i =
    if i < (s * 0.2) then i / (s * 0.2)
    else if i > (0.7 * s) then 1.0 - (i - (0.7 * s)) / ((1.0 - 0.7) * s)
    else 1.0

sineOscillator :: Float -> Float -> Float
sineOscillator f i = sin (f * 2 * pi * i / 44100)

am :: Float -> Float -> Float
am f i = 0.5 * (1.0 + sin (f * 2 * pi * i / 44100))


valueOf :: Int -> Int
valueOf x = x


encodeFloats :: [Float] -> BL.ByteString
encodeFloats fs = runPut $ mapM_ (putWord32le . castFloatToWord32) fs


app req respond = do
    let lower = sampleRate * 6
        upper = sampleRate * 20

    v <- randomRIO (lower, upper)

    let qText = queryToQueryText (queryString req)
        mNText :: Maybe T.Text
        mNText = lookup "note" qText >>= id
        mNInt :: Maybe Int
        mNInt = mNText >>= readMaybe . T.unpack
        note =
          case mNInt of
            Just n -> n
            Nothing -> 0
        samples = valueOf v
        f = frequencyOf note
        msg = "Haskell " ++ show note ++ " at " ++ show f ++ "Hz for " ++ show (quotientOf samples sampleRate) ++ "s"
        waveFunctions = [sineOscillator f, amplitude (fromIntegral samples), sineOscillator 2.0]

        w = map (\s -> foldl (*) 1.0 (map ($ s) waveFunctions)) (map fromIntegral [0..samples])

        body = encodeFloats w
        len = BL.length body
        lenBs = BS.pack (show len)

    putStrLn (msg)
    respond $ responseLBS status200 [(hContentType, "/plain"), (hContentLength, lenBs)] body



main :: IO ()
main = do
    [portStr] <- getArgs
    let port = read portStr :: Int
    putStrLn ("Haskell listening on port " ++ show port)
    run port app
