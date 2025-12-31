{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType, hContentLength)


sineOscillator :: Float -> Float -> Float
sineOscillator f i = sin (f * 2 * pi * i / 44100)

main :: IO ()
main = run 9964 $ \_req respond ->
  respond $ responseLBS status200 [(hContentType, "text/plain"), (hContentLength, "13")] "Hello Andrew\n"
