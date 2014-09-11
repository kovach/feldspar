module FT.Tests where

import qualified Prelude
import Feldspar as F
import Feldspar.Vector
import Feldspar.Compiler as C
import Feldspar.Matrix as M
import Feldspar.Compiler.Backend.C.Options as O

testm2 :: Matrix Float -> Matrix Float -> Matrix Float
testm2 b c = b *** c

testm3 :: Matrix Float -> Matrix Float -> Matrix Float -> Matrix Float
testm3 a b c = a *** b *** c

-- TODO 'between' doesn't do anything?
sum3 :: Vector1 Int32 -> Data Int32
sum3 = sum -:: (between 3 3 |> between 0 0) >-> id

