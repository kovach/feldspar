module FT.Experiments where

import qualified Prelude as P
import Feldspar as F
import Feldspar.Vector
--import Feldspar.Vector.Internal -- TODO
import Feldspar.Compiler as C
import Feldspar.Matrix as M
import Feldspar.Compiler.Backend.C.Options as O

testm2 :: Matrix Float -> Matrix Float -> Matrix Float
testm2 b c = b *** c

testm3 :: Matrix Float -> Matrix Float -> Matrix Float -> Matrix Float
testm3 a b c = a *** b *** c

sum3 :: Vector1 Int32 -> Data Int32
sum3 = sum -:: newLen 3 >-> id

--for1 :: Data Length -> Matrix Float -> Vector1 Float
--for1 :: Data Length -> Matrix Float -> Data [Float]
for1 =
  let
    len = 10
  in 
   --thawVector $
   runMutable$
     do 
       arr <- newArr len (1 :: Data Float)
       mat <- newArr len arr
       forM len (\i -> do return ()
                          --mij <- mat !! (i,i)
                          --(mat <~) i 0 mij
                       )
       return ()
       --mat' <- freezeArray mat
       --P.mapM freezeArray mat'



v1 :: Data Length -> Vector1 Float
v1 n = 
 let
   indf :: Data Index -> Data Float
   indf = (\i -> condition (i == 0) 0 (v ! (i-1) + 1))
   v =
     indexed n indf
 in v
