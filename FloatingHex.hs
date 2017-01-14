module FloatingHex (hf, showHFloat) where

import Data.Ratio ((%))
import Numeric    (showHex)

import qualified Language.Haskell.TH.Syntax as TH
import           Language.Haskell.TH.Quote

parseHexFloat :: String -> Maybe Double
parseHexFloat = go0
  where go0 ('0':'x':rest) = go1 rest
        go0 ('0':'X':rest) = go1 rest
        go0 _              = Nothing

        go1 cs = case break (== 'p') cs of
                   (pre, 'p':'+':d) -> go2 pre d
                   (pre, 'p':    d) -> go2 pre d
                   _                -> Nothing

        go2 cs = case break (== '.') cs of
                   (pre, '.':post) -> construct pre post
                   _               -> construct cs  ""

        rd :: Read a => String -> Maybe a
        rd s = case reads s of
                 [(x, "")] -> Just x
                 _         -> Nothing

        construct pre post d = do a <- rd $ "0x" ++ pre ++ post
                                  e <- rd d
                                  return $ val a (length post) e

        val :: Integer -> Int -> Integer -> Double
        val a b e
          | e > 0 = fromRational $ (top * expt) % bot
          | True  = fromRational $ top % (expt * bot)
          where top, bot, expt :: Integer
                top  = a
                bot  = 16 ^ b
                expt =  2 ^ abs e

hf :: QuasiQuoter
hf = QuasiQuoter { quoteExp  = q
                 , quotePat  = p
                 , quoteType = error "Unexpected hexadecimal float in a type context"
                 , quoteDec  = error "Unexpected hexadecimal float in a declaration context"
                 }
   where q :: String -> TH.Q TH.Exp
         q s = case parseHexFloat s of
                  Just d  -> TH.lift d
                  Nothing -> fail $ "Invalid hexadecimal floating point number: |" ++ s ++ "|"

         p :: String -> TH.Q TH.Pat
         p s = case parseHexFloat s of
                  Just d  -> return (TH.LitP (TH.RationalL (toRational d)))
                  Nothing -> fail $ "Invalid hexadecimal floating point number: |" ++ s ++ "|"

showHFloat :: RealFloat a => a -> ShowS
showHFloat x
 | isNaN x          = showString "nan"
 | isInfinite x     = showString $ if x > 0 then "+inf" else "-inf"
 | isNegativeZero x = showString "-0x0p1"
 | x < 0            = showString $ "-0x" ++ body
 | True             = showString $ "0x"  ++ body
 where (m, n)     = decodeFloat (abs x)
       pre        = showHex m ""
       (pre', l)  = case pre of
                     ""    -> error $ "impossible happened! " ++ show (pre, m)
                     (f:p) -> (f : trim p, length p)
       trim s = case dropWhile (== '0') (reverse s) of
                  "" -> ""
                  t  -> "." ++ reverse t
       body   = pre' ++ "p" ++ show (n + 4 * l)
