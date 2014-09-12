module FT.UDU where

import qualified Prelude
import Feldspar as F
import Feldspar.Vector
import qualified Feldspar.Stream as S
import Feldspar.Matrix as M

import Feldspar.Compiler as C

import FT.Util
import FT.Tests

de :: Vector1 Float -> Matrix Float -> Matrix Float
de e0 sats =
  flip map sats (`diff` e0)

vplus v1 v2 = zipWith (+) v1 v2
dot v1 v2 = sum $ zipWith (*) v1 v2
range = indexed id

ddp e0 sats b epsilon =
  (de e0 sats) *** b `vplus` epsilon

unfold_base :: Data Length
            -> Data Float
            -> (Data WordN -> Data Float -> Data Float)
            -- -> Data [Float]
            -> Vector1 Float
unfold_base n base fn =
  let step (i, p) = 
        condition (i == 0)
            (let a = base in
             (a, (1, a)))
            (let a = fn i p in
             (a, (i+1, a)))
  in
  -- TODO sugar?
  thawVector $ sugar $ S.take n $ S.unfold step (0, 0)


udu_update :: Data Length
           -> Matrix Float
           -> Vector1 Float
           -> Data Float
           -> Vector1 Float
           -> Vector1 Float
           -> Vector1 Float
udu_update n u d r g f =
  let 
    gamma =
      unfold_base 
        n
        (r + g ! 0 * f ! 0)
        (\j gamma -> gamma + g ! j * f ! j)

    d_bar =
      unfold_base
        n
        ((d ! 0) * r / (gamma ! 0))
        (\i _ -> d ! i * gamma ! (i-1) / gamma ! i)
    u_bar = drop 1 $ indexed $ \i ->
              u ! i `diff` (map ((* f ! i) . (/ gamma ! (i-1)))
                                $ k ! (i-1))
    -- TODO fix k
    -- eliminate scan
    -- why doesn't scan fuse?
    k0 = indexed $ \i -> cond (i == 0) (g!0) 0
    k = scan (\k ind -> k + g ! ind * u ! ind) k0 (range n)
  in gamma

