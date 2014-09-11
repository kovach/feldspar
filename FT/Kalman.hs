module FT.Kalman where

import qualified Prelude
import Feldspar as F
import Feldspar.Vector
import Feldspar.Matrix as M
import Feldspar.Core.Frontend.FFI as FFI

-- -- --
-- KF --
-- -- --

-- f : model,
-- x : state estimate,
-- p : covariance estimate,
-- q : process noise covariance
predict f x p q =
  let x' = f *** x
      p' = (f ***
            p ***
            (transpose f))
           .+ q
  in
    (x', p')

-- h : observation matrix
-- r : observation noise covariance
-- z : observed state value
update :: Matrix Float -> Matrix Float -> Matrix Float -> Matrix Float
       -> Matrix Float -> Matrix Float -> Matrix Float
       -> (Matrix Float, Matrix Float)
update f x p q h r z =
  let
    (x', p') = predict f x p q
    -- HPH^ + R
    residual = z .- h *** x'
    residual_covariance = h *** p' *** (transpose h) .+ r

    -- TODO use FFI (see below)
    inverse' = transpose

    kalman_gain = p' *** (transpose h) *** (inverse' residual_covariance)


    x'' = x' .+ (kalman_gain *** residual)
    p'' = p' .- (kalman_gain *** h *** p')
  in
    (x'', p'')

-- TODO FFI. How to generate Denotation?
inverse :: Matrix a -> Matrix a
inverse = undefined -- foreignImport "inverse" undefined

-- TODO use a type patch instead?
predictF :: Matrix Float -> Matrix Float -> Matrix Float -> Matrix Float
         -> (Matrix Float, Matrix Float)
predictF = predict

