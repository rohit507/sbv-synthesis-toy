
{-|
Module      : ConstraintExpr
Description : AST for arbitrary z3 compatible constraints
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

We define a type that (when used with the 'Free' monad) gives us an AST for
expressions over constraints of boolean variables, simple enumerables,
and linear equations.

|-}

module ConstraintExpr where

-- * Design Motivation
--
-- We want to be able to:
--
--  - Express variables and constraints in a nice, human-readable manner
--  - Generate a data structure that we can use for simplification and analysis
--  - Convert the created data structure into various formats, like a graph,
--    input for an SMT solver, pretty printed ASCII art, etc..
--
-- To do this we use what is known as the "free monoid + interpreter" pattern
-- (See https://www.tweag.io/posts/2018-02-05-free-monads.html ).
-- Which allow us to store complex expressions and then run various
-- interpreters/algebras over those expressions.
--
-- To slightly complicate matters, we use Generalized Algebraic DataTypes (GADTs)
-- to add an extra layer of type safety to the process
-- (See http://jstimpfle.de/blah/free-monads-gadts.html ).

-- | Constraint Expressions
--
--   Datatype for type safe expressions of contraints. We use a set of
--   typeclasses to restrict the system to types that are usable in the context
--   of SMT Solving.
--
--   the first type operator 'sym :: Type -> Type' is a wrapper that says
--   "Hey, I'm not an actual 'a'! I'm a symbolic version of an 'a'."
--   Wherever you define for 'sym' one should be extra careful to ensure that
--   you only allow the user to access the constructors for types that you can
--   actually handle symbolically.
--
--   All infix type constructors must start with a colon.
data CExpr sym a where

  -- | Numeric Operations
  (:+) :: CNum a => CExpr s (s a) -> CExpr s (s a) -> CExpr s (s a)
  (:-) :: CNum a => CExpr s (s a) -> CExpr s (s a) -> CExpr s (s a)

  -- | Sum of all the variables in the list
  Sum :: CNum a => [CExpr s (s a)] -> CExpr s (s a)

  -- | Since we are limiting ourselves to linear operations, one parameter of
  --   every multiplication operation should be a constant value.
  --
  --   We are not creating an operator for division since I don't want to
  --   deal with that bullshit.
  (:*) :: CNum a => a -> CExpr s (s a) -> CExpr s (s a)

  -- | Equality Operations
  (:==) :: CEq a => CExpr s (s a) -> CExpr s (s a) -> CExpr s (s Bool)
  (:/=) :: CEq a => CExpr s (s a) -> CExpr s (s a) -> CExpr s (s Bool)

  -- | Boolean Operations
  (:!) :: CExpr s (s Bool) -> CExpr s (s Bool)
  (:^) :: CExpr s (s Bool) -> CExpr s (s Bool) -> CExpr s (s Bool)
  (:&&) :: CExpr s (s Bool) -> CExpr s (s Bool) -> CExpr s (s Bool)
  (:||) :: CExpr s (s Bool) -> CExpr s (s Bool) -> CExpr s (s Bool)

  -- | Logical Implication
  (:=>) :: CExpr s (s Bool) -> CExpr s (s Bool) -> CExpr s (s Bool)

  -- | Logical Equality
  (:<=>) :: CExpr s (s Bool) -> CExpr s (s Bool) -> CExpr s (s Bool)

  -- | All of the following in the list must be true
  All :: [CExpr s (s Bool)] -> CExpr s (s Bool)

  -- | Any of the following must be true
  Any :: [CExpr s (s Bool)] -> CExpr s (s Bool)

  -- | Count how many are true
  Count :: [CExpr s (s Bool)] -> CExpr s (s Int)

  -- | Ordering Operations
  (:<) :: COrd a => CExpr s (s a) -> CExpr s (s a) -> CExpr s (s Bool)
  (:<=) :: COrd a => CExpr s (s a) -> CExpr s (s a) -> CExpr s (s Bool)
  (:>) :: COrd a => CExpr s (s a) -> CExpr s (s a) -> CExpr s (s Bool)
  (:>=) :: COrd a => CExpr s (s a) -> CExpr s (s a) -> CExpr s (s Bool)

  -- | Maximum of the set
  Max :: COrd a => [CExpr s (s a)] -> CExpr s (s a)

  -- | Minimum of the set
  Min :: COrd a => [CExpr s (s a)] -> CExpr s (s a)

  -- | This lets us store a name for any expression.
  IsNamed :: String -> CExpr s (s a) -> CExpr s (s a)

  -- | This lets us add a comment to any particular expression, it's mostly
  --   an aid with debugging. In general most
  WithComment :: CExpr s (s a) -> String -> CExpr s (s a)

-- * Shadow Typeclasses
--
-- The classes for types with equality (Eq), integers (Num),
-- ordering (Ord), and reals (Real) require us to implement functions that
-- don't make much sense in the context of a constraint expression.
--
-- The shadow classes below give us alternates that don't have those
-- same restrictions.

-- | Shadow 'Eq' typeclass
class CEq a

-- | Shadow 'CNum' typeclass
class CNum a

-- | Shadow 'Ord' typeclass
class COrd a

-- | Shadow 'Real' typeclass
class CReal a

-- | Class for symbolic variables
class CVar a



{-

Table of existing operators, with their binding strengths and associativity

Prec-   Left associative        Non-associative         Right associative
edence  operators               operators               operators
9       !!                                               .
8                                                       ^ , ^^ , ⋆⋆
7       ⋆, /, ‘div‘,
        ‘mod‘, ‘rem‘, ‘quot‘
6       +, -
5                                                       :, ++
4                               ==, /=, <, <=, >, >=,
                               ‘elem‘, ‘notElem‘
3                                                       &&
2                                                       ||
1       >>, >>=
0                                                       $, $!, ‘seq‘

-}
