-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Distribution.Continuous
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  GPL-style
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL continuous random distribution functions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Distribution.Continuous (
                                 ZeroParamDist(..), OneParamDist(..)
                                , TwoParamDist(..), ThreeParamDist(..)
                                , MultiParamDist(..)
                                , BivariateDist(..)
                                , DistFunc(..)
                                , random_0p, density_0p
                                , random_1p, density_1p
                                , random_2p, density_2p
                                , random_3p, density_3p
                                , random_mp, density_mp
                                , random_biv, density_biv
                                , spherical_vector 
                             ) where

-----------------------------------------------------------------------------

import Data.Packed.Vector
--import Data.Packed.Matrix hiding(toLists)
import Data.Packed.Development

--import Numeric.LinearAlgebra.Linear

--import Control.Monad(when)

import Foreign hiding(shift)
--import Foreign.ForeignPtr
--import Foreign.Marshal.Alloc(alloca)
--import Foreign.C.Types(CInt,CChar)
import Foreign.C.Types(CInt(..))
--import Foreign.C.String(newCString,peekCString)

--import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)

--import GHC.Base
--import GHC.IOBase

--import Prelude hiding(reverse)

import Numeric.GSL.Distribution.Common

import System.IO.Unsafe(unsafePerformIO)

-----------------------------------------------------------------------------

data ZeroParamDist = Landau
                   deriving Enum

data OneParamDist = Gaussian    -- ^ standard deviation
                  | Exponential -- ^ mean
                  | Laplace     -- ^ width
                  | Cauchy      -- ^ scale
                  | Rayleigh    -- ^ standard deviation
                  | ChiSq       -- ^ degrees of freedom
                  | TDist       -- ^ degrees of freedom
                  | Logistic    -- ^ scale
                    deriving Enum

data TwoParamDist = GaussianTail -- ^ limit, standard deviation
                  | ExpPower     -- ^ scale, exponent
                  | RayleighTail -- ^ lower limit, standard deviation
                  | Levy         -- ^ scale, exponent
                  | Gamma        -- ^ par1, par2
                  | Uniform      -- ^ lower, upper
                  | Lognormal    -- ^ offset, standard deviation
                  | FDist        -- ^ degrees of freedom, degrees of freedom
                  | Beta         -- ^ parameter a, parameter b
                  | Pareto       -- ^ exponent, scale
                  | Weibull      -- ^ scale, exponent
                  | GumbellI     -- ^ A, B
                  | GumbellII    -- ^ A, B
                    deriving Enum

data ThreeParamDist = LevySkew    -- ^ scale, exponent, skewness
                    deriving Enum

data MultiParamDist = Dirichlet   -- ^ size, alpha
                    deriving Enum

data BivariateDist = BiGaussian  -- ^ standard deviation, standard deviation, correlation coefficient
                    deriving Enum

-----------------------------------------------------------------------------

fromei x = fromIntegral (fromEnum x) :: CInt

-----------------------------------------------------------------------------

-- | draw a sample from a zero parameter distribution
random_0p :: ZeroParamDist    -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ result
random_0p d s = unsafePerformIO $ distribution_random_zero_param (fromIntegral s) (fromei d)            

-- | probability of a variate take a value outside the argument
density_0p :: ZeroParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Double         -- ^ value
                -> Double         -- ^ result
density_0p d f x = unsafePerformIO $ do
                                     case d of
                                            Landau -> density_only f d x
    where density_only f' d' x' = if f' /= Density
                                     then error "distribution has no CDF"
                                     else distribution_dist_zero_param (fromei f') (fromei d') x'

foreign import ccall "distribution-aux.h random0" distribution_random_zero_param :: CInt -> CInt -> IO Double
foreign import ccall "distribution-aux.h random0_dist" distribution_dist_zero_param :: CInt -> CInt -> Double -> IO Double

-----------------------------------------------------------------------------

-- | draw a sample from a one parameter distribution
random_1p :: OneParamDist    -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ parameter
          -> Double          -- ^ result
random_1p d s p = unsafePerformIO $
                  alloca $ \r -> do
                      check "random1p" $ distribution_random_one_param (fromIntegral s) (fromei d) p r
                      r' <- peek r
                      return r'

-- | probability of a variate take a value outside the argument
density_1p :: OneParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Double         -- ^ parameter
                -> Double         -- ^ value
                -> Double         -- ^ result
density_1p d f p x = unsafePerformIO $ distribution_dist_one_param (fromei f) (fromei d) x p

foreign import ccall "distribution-aux.h random1" distribution_random_one_param :: CInt -> CInt -> Double -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random1_dist" distribution_dist_one_param :: CInt -> CInt -> Double -> Double -> IO Double

-----------------------------------------------------------------------------

-- | draw a sample from a two parameter distribution
random_2p :: TwoParamDist    -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ parameter 1
          -> Double          -- ^ parameter 2
          -> Double          -- ^ result
random_2p d s p1 p2  = unsafePerformIO $
                       alloca $ \r -> do
                           check "random2p" $ distribution_random_two_param (fromIntegral s) (fromei d) p1 p2 r
                           r' <- peek r
                           return r'

-- | probability of a variate take a value outside the argument
density_2p :: TwoParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Double         -- ^ parameter 1
                -> Double         -- ^ parameter 2
                -> Double         -- ^ value
                -> Double         -- ^ result
density_2p d f p1 p2 x = unsafePerformIO $ do
                          case d of
                                 GaussianTail -> density_only f d p1 p2 x
                                 ExpPower     -> no_inverse f d p1 p2 x
                                 RayleighTail -> density_only f d p1 p2 x
                                 Levy         -> error "no PDF or CDF for Levy"
                                 _            -> distribution_dist_two_param (fromei f) (fromei d) x p1 p2
    where density_only f' d' p1' p2' x' = if f' /= Density
                                            then error "distribution has no CDF"
                                            else distribution_dist_two_param (fromei f') (fromei d') x' p1' p2'
          no_inverse f' d' p1' p2' x' = if (f' == LowInv || f' == UppInv)
                                          then error "distribution has no inverse CDF"
                                          else distribution_dist_two_param (fromei f') (fromei d') x' p1' p2'

foreign import ccall "distribution-aux.h random2" distribution_random_two_param :: CInt -> CInt -> Double -> Double -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random2_dist" distribution_dist_two_param :: CInt -> CInt -> Double -> Double -> Double -> IO Double

-----------------------------------------------------------------------------

-- | draw a sample from a three parameter distribution
random_3p :: ThreeParamDist  -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ parameter 1
          -> Double          -- ^ parameter 2
          -> Double          -- ^ parameter 3
          -> Double          -- ^ result
random_3p d s p1 p2 p3 = unsafePerformIO $
                         alloca $ \r -> do
                                 check "random_3p" $ distribution_random_three_param (fromIntegral s) (fromei d) p1 p2 p3 r
                                 r' <- peek r
                                 return r'

-- | probability of a variate take a value outside the argument
density_3p :: ThreeParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Double         -- ^ parameter 1
                -> Double         -- ^ parameter 2
                -> Double         -- ^ parameter 3
                -> Double         -- ^ value
                -> Double         -- ^ result
density_3p d _ _ _ _ _ = unsafePerformIO $ do
                            case d of
                                 LevySkew -> error "Levy skew has no PDF or CDF"
                                 _        -> error "unknown 3 parameter distribution"

foreign import ccall "distribution-aux.h random3" distribution_random_three_param :: CInt -> CInt -> Double -> Double -> Double -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random3_dist" distribution_dist_three_param :: CInt -> CInt -> Double -> Double -> Double -> Double -> IO Double

-----------------------------------------------------------------------------

-- | draw a sample from a three parameter distribution
random_mp :: MultiParamDist  -- ^ distribution type
          -> Int             -- ^ random seed
          -> Vector Double   -- ^ parameters
          -> Vector Double   -- ^ result
random_mp d s p = unsafePerformIO $ do
                  r <- createVector $ dim p
                  app2 (distribution_random_multi_param (fromIntegral s) (fromei d)) vec p vec r "random_mp"
                  return r

-- | probability of a variate take a value outside the argument
density_mp :: MultiParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Vector Double  -- ^ parameters
                -> Vector Double  -- ^ values
                -> Double         -- ^ result
density_mp d f p q = unsafePerformIO $ do
                            case d of
                                 Dirichlet -> density_only f d p q
    where density_only f' d' p' q' = if f' /= Density
                                              then error "distribution has no CDF"
                                              else alloca $ \r -> do
                                                                  app2 (distribution_dist_multi_param (fromei f') (fromei d') r) vec p' vec q' "density_mp"
                                                                  r' <- peek r
                                                                  return r'

foreign import ccall "distribution-aux.h random_mp" distribution_random_multi_param :: CInt -> CInt -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random_mp_dist" distribution_dist_multi_param :: CInt -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------

-- | draw a sample from a bivariate distribution
random_biv :: BivariateDist  -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ parameter 1
          -> Double          -- ^ parameter 2
          -> Double          -- ^ parameter 3
          -> (Double,Double) -- ^ result
random_biv d s p1 p2 p3 = unsafePerformIO $
                         alloca $ \r1 ->
                             alloca $ \r2 -> do
                                 distribution_random_bivariate (fromIntegral s) (fromei d) p1 p2 p3 r1 r2
                                 r1' <- peek r1
                                 r2' <- peek r2
                                 return (r1',r2')

-- | probability of a variate take a value outside the argument
density_biv :: BivariateDist      -- ^ density type
                -> DistFunc        -- ^ distribution function type
                -> Double          -- ^ parameter 1
                -> Double          -- ^ parameter 2
                -> Double          -- ^ parameter 3
                -> (Double,Double) -- ^ value
                -> Double          -- ^ result
density_biv d f p1 p2 p3 (x,y) = unsafePerformIO $ do
                            case d of
                                 BiGaussian -> density_only f d p1 p2 p3 x y
    where density_only f' d' p1' p2' p3' x' y' = if f' /= Density
                                                 then error "distribution has no CDF"
                                                 else distribution_dist_bivariate (fromei f') (fromei d') x' y' p1' p2' p3'

foreign import ccall "distribution-aux.h random_biv" distribution_random_bivariate :: CInt -> CInt -> Double -> Double -> Double -> Ptr Double -> Ptr Double -> IO ()
foreign import ccall "distribution-aux.h random_biv_dist" distribution_dist_bivariate :: CInt -> CInt -> Double -> Double -> Double -> Double -> Double -> IO Double

-----------------------------------------------------------------------------

-- | returns a normalised random direction vector from a multivariate gaussian distribution
spherical_vector :: Int           -- ^ seed
                 -> Int           -- ^ vector size
                 -> Vector Double -- result
spherical_vector s vs = unsafePerformIO $ do
                        r <- createVector vs
                        app1 (distribution_spherical_vector (fromIntegral s)) vec r "spherical_vector"
                        return r

foreign import ccall "distribution-aux.h spherical_vector" distribution_spherical_vector :: CInt -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
