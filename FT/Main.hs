module Main where

import qualified Prelude
import Feldspar as F
import Feldspar.Vector
import Feldspar.Compiler as C
import Feldspar.Matrix as M

import FT.Amb_kf as K
import FT.Util as U

main = U.chk K.assign_de_mtx_3 "assign_de_mtx"
