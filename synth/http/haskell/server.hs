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

frequencyOf :: Int -> Float
frequencyOf note = (2.0 ** (fromIntegral(note - 69) / 12.0)) * 440.0;

sineOscillator :: Float -> Int -> Float
sineOscillator f i = sin (f * 2 * pi * fromIntegral (i) / 44100)

wave :: Float -> Int -> [Float]
wave f n = map (\x -> sineOscillator f x) [0..n]

valueOf :: Int -> Int
valueOf x = x

quotientOf :: Int -> Int -> Float
quotientOf a b = (fromIntegral a) / (fromIntegral b)

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
        w = wave f samples
        body = encodeFloats w
        len = BL.length body
        lenBs = BS.pack (show len)

    putStrLn (msg)
    respond $ responseLBS status200 [(hContentType, "text/plain"), (hContentLength, lenBs)] body


main :: IO ()
main = run port app
