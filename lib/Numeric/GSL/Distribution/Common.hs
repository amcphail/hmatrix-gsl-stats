-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Distribution.Common
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL common data types for distributions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Distribution.Common (
                                DistFunc(..)
                             ) where

-----------------------------------------------------------------------------

data DistFunc = Density    -- ^ pdf
              | Lower      -- ^ lower cdf
              | Upper      -- ^ upper cdf
              | LowInv     -- ^ lower inverse cdf
              | UppInv     -- ^ upper inverse cdf
                deriving(Enum,Eq)

-----------------------------------------------------------------------------
