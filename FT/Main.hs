module Main where


import qualified Prelude
import Feldspar as F
import Feldspar.Vector hiding ((**))
import Feldspar.Compiler as C
import Feldspar.Compiler.Plugin as CP
import Feldspar.Matrix as M

import FT.Kalman

main = icompile predictF
