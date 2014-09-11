module FT.Amb_kf where

import qualified Prelude
import Feldspar as F
import Feldspar.Vector
import Feldspar.Matrix as M

import FT.Util

-- -------------------------- --
-- Fully copied from amb_kf.c --
-- -------------------------- --
assign_de_mtx :: Matrix Float -> Vector1 Float -> Matrix Float
assign_de_mtx sats_with_ref_first ref_ecef =
  let
    sats = sats_with_ref_first
    num_sats = length sats

    delta0 = force $
             diff (sats ! 0) ref_ecef
    norm0  = sumSq delta0
    e0     = map (/ norm0) delta0

    sats'  = drop 1 sats
    de     = flip map sats' $ \sat ->
              let delta = force $
                          diff sat ref_ecef
                  norm  = sumSq delta
              in 
                diff (map (/ norm) delta) e0
  in
    de

-- TODO finish
residual_obs_cov :: Data WordN -> Matrix Int32
residual_obs_cov num_dds = -- phase_var code_var =
  let
    qt0 = indexedMat 2 2 (\i j -> 0)
    qt1 = forLoop num_dds qt0 (\ind mat -> mat)
    qt :: Matrix Int32
    qt = undefined

    --arr = runMutable $ do
    --        forArr 10 (\ind -> ind)
  --in (qt1, arr)
  in qt1

