module Main where

import System.IO ( stdin, hGetContents, stderr)
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexCmm
import ParCmm
import Interpreter
import PrintCmm
import AbsCmm
import ErrM
import qualified Exec

main = do
  interact (showResult . prog)

--showResult (Ok s) = show s
showResult (Ok s) = foldl (\a b -> a ++ b ++ "\n") [] (reverse (Exec.outs s))

showResult (Bad s) = s ++ "\n"

prog ins =
  do
    me <- pProgram (myLexer ins)
    s <- Exec.empty
    s <- Exec.loadBuiltins s
    s <- transProgram me s
    Exec.runMain s