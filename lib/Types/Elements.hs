
{-|
Description : This is where we keep the data-structures for representing the
              elements of a design. (i.e. Libraries, Designs, Blocks,
              Ports, and Links)
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

These types let us represent different complex structures we are working with.
|-}

module Types.Elements where

import Data.HashMap.Strict (HashMap)

-- * Library

-- | Libraries store a pile of uninstantiated ports, blocks, and libraries
--   which can be instantiated as we insert them into a design.
--
--   Type Parameters:
--      id := The type of identifiers we use to name elements of a design.
--      n  := The type of whatever data we store with a variable instantiating.
--      c  := The type of constraint expressions.
data Library id n c = Library {
       blocks :: HashMap id (Block id n c)
     , links  :: HashMap id (Link  id n c)
     } deriving (Show, Read)

-- * Design

-- | Designs are a set of instantiated variables and constraints that can be
--   solved, analyzed, or whatever. Generally we instantiate parts and links
--   from the library and then add additional variables and constraints to
--   generate an actual constraint satisfaction problem.
data Design id n c = Design {
       blocks      :: HashMap id (Block id n c)
     , links       :: HashMap id (Link  id n c)
     , variables   :: HashMap id n
     , constraints :: HashMap id c
     } deriving (Show, Read)

-- * Blob

-- | Blobs are an amorphous blob of variables and their related constraints.
--   It provides a good target format to use when stripping irrelevant information
--   from a design and passing it to a solver.
data Blob id n c = Blob {
       variables   :: HashMap id n
     , constraints :: HashMap id c
     } deriving (Show, Read)

data Block id n c = Block {
       variables   :: HashMap id n
     , constraints :: HashMap id c
     , ports       :: HashMap id (Port id n c)
     } deriving (Show,Read)

data Link id n c = Link {
       variables   :: HashMap id n
     , constraints :: HashMap id c
     , ports       :: HashMap id (Port id n c)
     } deriving (Show,Read)

data Port id n c = Port {
       variables   :: HashMap id n
     , constraints :: HashMap id c
     } deriving (Show,Read)
