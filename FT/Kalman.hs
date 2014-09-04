module FT.Kalman where

import qualified Prelude
import Feldspar as F
import Feldspar.Vector hiding ((**))
import Feldspar.Compiler as C
import Feldspar.Compiler.Plugin as CP
import Feldspar.Matrix as M

import Feldspar.Core.Frontend.FFI as FFI

-- (***) =  mulMat

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
    kalman_gain = p' *** (transpose h) *** (inverse residual_covariance)

    x'' = x' .+ (kalman_gain *** residual)
    p'' = p' .- (kalman_gain *** h *** p')
  in
    (x'', p'')

-- TODO FFI?
inverse :: Matrix a -> Matrix a
inverse = undefined -- foreignImport "inverse" undefined


-- TODO use a type patch instead?
predictF :: Matrix Float -> Matrix Float -> Matrix Float -> Matrix Float
         -> (Matrix Float, Matrix Float)
predictF = predict 
