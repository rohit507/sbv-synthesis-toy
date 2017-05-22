module Main where

import Types
import Synthesis
import Extract
import Library

import Data.SBV

import Development.Shake
import Development.Shake.FilePath

import Control.Monad

main :: IO ()
main = shakeArgs shakeOptions $
  phony "simpletest" $
    liftIO $ print <=< satWith z3{getUnsatCore=True} $ do
      (r,model) <- runSymb $ do
        -- Add all of our parts, incl duplicates
        testSpec
        swLink
        testSource
        -- Instrument
        addEdges
        -- Finish up
        addFinalConstraints
        return . literal $ True
      return r

-- Primary targets are going to various problems, with a specific set of 
-- possible flags that we can use to run them as follows: 
--
--  - With iteration, with 
