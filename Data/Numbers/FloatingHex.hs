-------------------------------------------------------------------------
-- |
-- Module      :  Data.Numbers.FloatingHex
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Reading/Writing hexadecimal floating-point numbers.
--
-- See: <http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf>, pages 57-58.
-- We slightly diverge from the standard and do not allow for the "floating-suffix,"
-- as the type inference of Haskell makes this unnecessary.
-----------------------------------------------------------------------------
module Data.Numbers.FloatingHex (hf, showHFloat) where

import Data.Char  (toLower)
import Data.Ratio ((%))
import Numeric    (showHex)

import qualified Language.Haskell.TH.Syntax as TH
import           Language.Haskell.TH.Quote

-- | Turn a hexadecimal float to an internal double, if parseable.
parseHexFloat :: String -> Maybe Double
parseHexFloat = go0 . map toLower
  where go0 ('0':'x':rest) = go1 rest
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

-- | A quasiquoter for hexadecimal floating-point literals.
-- See: <http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf>, pages 57-58.
-- We slightly diverge from the standard and do not allow for the "floating-suffix,"
-- as the type inference of Haskell makes this unnecessary.
--
-- Example:
--
--  > {-# LANGUAGE QuasiQuotes #-}
--  > import Data.Numbers.FloatingHex
--  >
--  > f :: Double
--  > f = [hf|0x1.f44abd5aa7ca4p+25|]
--
--  With these definitions, @f@ will be equal to the number @6.5574266708245546e7@
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

-- | Show a floating-point value in the hexadecimal format.
--
-- NB. While this function will print a faithful (i.e., correct) value, it is
-- not 100% compatible with the @%a@ modifier as found in the C's printf implementation.
--
-- >>> showHFloat (212.21 :: Double) ""
-- "0x1.a86b851eb851fp7"
-- >>> showHFloat (-12.76 :: Float) ""
-- "-0xc.c28f6p0"
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
