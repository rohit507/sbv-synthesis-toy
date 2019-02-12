
{-|
Module      : Internal.EmbeddingDSL
Description : Convenient ways to express element definitions
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3k
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

This is where we use some syntactic tricks to make writing definitions easier.

You should be editing this if you changed the type of any of the elements
in 'Types.Elements'
-}

module Internal.EmbeddingDSL where

import Data.HashMap.Strict (HashMap)
import Types.Elements
-- | A separate type so we don't confuse Identifiers with other
--   uses of String.
--
--   The strings should be from /[a-z,A-Z]+/
newtype Id = Id {getId :: String}
  deriving (Show, Read)

-- | Paths are the relative path from the root of the Library/Design to this
--   particular element.
newtype Path id = Path [id]
  deriving (Show, Read)


-- *
-- = Assembling Elements with Elegance
--
-- We have these definitions for element types, Block, Link, and Port.
-- By default, complex data structures or ASTs are not something you would
-- write as literals. Doing this for a data structure that represents code is
-- even worse.
--
-- So we could make this a full fledged DSL, with a syntax, parser and whatnot.
-- But I'm lazy, so no.
--
-- I'd much rather exploit the infrastructure around this language, haskell, to
-- create a nice input format for everything.
-- Thankfully haskell has a few very nice abstractions that we can use to do
-- this.
--
-- The one we're going to be using in this c




--  new port

-- in point of fact we'll end up with monads whose state is basically just the
-- thing we're constraining. Observable sharing might be useful to decompress
-- things later on.

-- so I think I'll have to lean (and rather hard) on explicitly defining
-- variable names, and having references work with a string

-- port :: string -> PortM a -> BlockM a .. store context from top down
--
-- infixOps
-- +: -: *: sum eq
