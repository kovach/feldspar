module FT.Util where

import qualified Prelude
import Feldspar as F
import Feldspar.Vector
import Feldspar.Compiler as C
import Feldspar.Matrix as M
import Feldspar.Compiler.Backend.C.Options as O

diff :: (Type a, Numeric a) => Vector1 a -> Vector1 a -> Vector1 a
diff = zipWith (-)

sumSq :: (Type a, Numeric a) => Vector1 a -> Data a
sumSq = sum . map (\x -> x * x)

dist :: (Type a, Numeric a) => Vector1 a -> Vector1 a -> Data a
dist v1 v2 = sumSq $ diff v1 v2

repeat :: Type a => Data Length -> Vector1 a -> Matrix a
repeat rows v = indexedMat rows (length v) (\i j -> v ! j)

-- Compiler convenience
chk t name = icompile' options name t
  where
    options = C.defaultOptions
      -- { memoryInfoVisible = True -- TODO doesn't do anything?
      -- , unroll = Unroll 8 }
