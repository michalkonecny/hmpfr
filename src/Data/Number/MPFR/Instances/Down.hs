-- {-# LANGUAGE MagicHash, CPP #-}
{-|
    Module      :  Data.Number.MPFR.Instances.Down
    Description :  Instance declarations
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

  This module defines instances 'Num', 'Real', 'Fractional', 'Floating' and 'RealFrac' of 'MPFR'.
  Operations are rounded with 'RoundMode' 'Down' and computed with maximum precision of two 
  operands or with the precision of the operand. 
-}


module Data.Number.MPFR.Instances.Down ()
where

import qualified Data.Number.MPFR.Arithmetic as A
import qualified Data.Number.MPFR.Special as S
import Data.Number.MPFR.Misc
import Data.Number.MPFR.Assignment
import Data.Number.MPFR.Comparison
import Data.Number.MPFR.Internal
import Data.Number.MPFR.Conversion
import Data.Number.MPFR.Integer

import Data.Maybe

import Data.Ratio


-- #if (__GLASGOW_HASKELL__ >= 610) && (__GLASGOW_HASKELL__ < 612)
-- import GHC.Integer.Internals
-- #elif __GLASGOW_HASKELL__ >= 612
-- import GHC.Integer.GMP.Internals
-- #endif

-- import qualified GHC.Exts as E

instance Num MPFR where
    d + d'        = A.add Down (maxPrec d d') d d'
    d - d'        = A.sub Down (maxPrec d d') d d'
    d * d'        = A.mul Down (maxPrec d d') d d'
    negate d      = A.neg Down (getPrec d) d
    abs d         = A.absD Down (getPrec d) d
    signum        = fromInt Down minPrec . fromMaybe (-1) .sgn
    fromInteger i = 
        fromIntegerA Down (max minPrec $ 1 + bitsInInteger i) i
--    fromInteger (S# i) = fromInt Down minPrec (E.I# i)
--    fromInteger i@(J# n _) = fromIntegerA Zero (fromIntegral . abs $ E.I# n * bitsPerIntegerLimb) i 

instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional MPFR where
    d / d'         = A.div Down (maxPrec d d') d d'
    fromRational r = fromInteger n / fromInteger d
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = S.pi Down 53
    exp d        = S.exp Down (getPrec d) d
    log d        = S.log Down (getPrec d) d
    sqrt d       = A.sqrt Down (getPrec d) d 
    (**) d d'    = A.pow Down (maxPrec d d') d d'
    logBase d d' = Prelude.log d' / Prelude.log d
    sin d        = S.sin Down (getPrec d) d
    cos d        = S.cos Down (getPrec d) d
    tan d        = S.tan Down (getPrec d) d
    asin d       = S.asin Down (getPrec d) d
    acos d       = S.acos Down (getPrec d) d
    atan d       = S.atan Down (getPrec d) d
    sinh d       = S.sinh Down (getPrec d) d
    cosh d       = S.cosh Down (getPrec d) d
    tanh d       = S.tanh Down (getPrec d) d
    asinh d      = S.asinh Down (getPrec d) d
    acosh d      = S.acosh Down (getPrec d) d
    atanh d      = S.atanh Down (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Down (getPrec d) d
