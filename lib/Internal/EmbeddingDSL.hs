
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

If you make changes to either the expression language or the element types,
you will probably have to make changes here to make those new features
easily accessible.
-}

module Internal.EmbeddingDSL where

data BlockM id n c l

data LinkM  id n c l

data PortM  id n c l


-- in all
--  new var
--  new lit
--  new expr

-- in block and link
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
