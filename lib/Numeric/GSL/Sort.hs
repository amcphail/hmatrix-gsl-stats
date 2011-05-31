-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Sort
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  GPL-style
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL sorting functions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Sort (
                         sort
                         ) where


import Data.Packed.Vector
import Data.Packed.Development

import Foreign
import Foreign.C.Types(CInt)

-- | sort the elements of a vector into ascending order
sort :: Vector Double -> Vector Double
sort v = unsafePerformIO $ do
         r <- createVector (dim v)
         app2 sort_sort vec v vec r "sort"
         return r

foreign import ccall "sort-aux.h sort" sort_sort :: CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt
