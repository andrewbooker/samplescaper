{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType, hContentLength)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)


sineOscillator :: Float -> Float -> Float
sineOscillator f i = sin (f * 2 * pi * i / 44100)

app req respond = do
    let body = BL.fromStrict . TE.encodeUtf8 $ T.concat ["Generating ", "57", "\n"]
    respond $ responseLBS status200 [(hContentType, "text/plain"), (hContentLength, "14")] body

main :: IO ()
main = run 9964 app
