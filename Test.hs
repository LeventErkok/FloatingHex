{-# LANGUAGE QuasiQuotes #-}
import FloatingHex

f :: Double
f = [hf|0x1.f44abd5aa7ca4p+500|]

g :: Float -> String
g [hf|0x1p1|]  = "two!"
g [hf|0x1p-1|] = "half!"
g d            = "something else: " ++ show d
