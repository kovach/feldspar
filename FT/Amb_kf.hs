module FT.Amb_kf where

import qualified Prelude
import Feldspar as F
import Feldspar.Vector
import Feldspar.Matrix as M

import Feldspar.Compiler as C

import FT.Util
-- TODO remove:
import FT.Tests
import FT.Kleene

-- -------------------------- --
-- Fully copied from amb_kf.c --
-- -------------------------- --
assign_de_mtx :: Data WordN -> Vector2 Float -> Vector1 Float -> Matrix Float
assign_de_mtx num_sats sats_with_ref_first ref_ecef =
  let
    -- Assume that there will be at least 1 sat
    sats = newLen (notBelow 1 num_sats) sats_with_ref_first

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

-- Specialize to 3d vectors
-- Use `map' to change width of matrix (second argument)
assign_de_mtx_3 =
  assign_de_mtx -:: id               -- num_sats
                >-> (map (newLen 3)) -- sats_list
                >-> newLen 3         -- ref
                >-> id               -- output

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

