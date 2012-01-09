-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.BasisSplines
-- Copyright   :  (c) A. V. H. McPhail 2011
-- License     :  GPL-style
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL statistics functions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.BasisSplines (
                ) where

-----------------------------------------------------------------------------

import Data.Packed.Vector
--import Data.Packed(Container(..))

--import Data.Packed.Development

--import Numeric.GSL.Vector
--import Numeric.LinearAlgebra.Instances()
--import Numeric.LinearAlgebra.Linear(Linear(..))

import Foreign
import Foreign.C.Types(CInt)
--import Foreign.Marshal.Alloc(alloca)

-----------------------------------------------------------------------------

type PD = Ptr Double

-----------------------------------------------------------------------------

getD1 f s v = unsafePerformIO $ do
              alloca $ \r -> do
                   app1 (f r) vec v s
                   r' <- peek r
                   return r'

getD2 f s v w = unsafePerformIO $ do
                alloca $ \r -> do
                app2 (f r) vec v vec w s
                r' <- peek r
                return r'

-----------------------------------------------------------------------------



-----------------------------------------------------------------------------

