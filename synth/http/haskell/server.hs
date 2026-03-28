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
import System.Random


port = 9964
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
    gen <- randomIO :: IO Float
    v <- randomRIO (sampleRate * 6, sampleRate * 14)

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
        t = quotientOf samples sampleRate
        msg = "Haskell " ++ show note ++ " at " ++ show f ++ "Hz for " ++ show t ++ "s"
        waveFunctions = [sineOscillator f]
        amplitudeFunctions = [amplitude (fromIntegral samples)]
        wb = foldl (.) (1.0*) waveFunctions
        ab = foldl (.) (1.0*) amplitudeFunctions
        sf = map fromIntegral [0..samples]
        w = map (\x -> wb (x) * ab (x) * (am 2.0) x) sf
        body = encodeFloats w
        len = BL.length body
        lenBs = BS.pack (show len)

    putStrLn (msg)
    respond $ responseLBS status200 [(hContentType, "/plain"), (hContentLength, lenBs)] body


main :: IO ()
main = run port app
