
{-|
Module      : ConstraintExpression
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

module ConstraintExpression where

import Data.Functor.Foldable (Fix)

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

-- | Expression Name
--
--   We use a type synonym/type alias to say that (for now) we are using
--   'String's as names for types. In the future you may wish to change it
--   and when you do, the compiler will complain about the generated errors,
--   making it easy to find and fix them.
type ExprName = String

-- | Comment
--
--   We use a type synonym/type alias to say that (for now) we are using
--   'String's to store comments.
type Comment = String

-- | Constraint Expression
--
data CExprF f
  = Add f f               -- ^ Addition
  | Sub f f               -- ^ Subtraction
  | Negate f              -- ^ Unary Negation
  | Mult f f              -- ^ Multiplication
  | Sum [f]               -- ^ Sum of all elements in a list
  | Eq f f                -- ^ Equality
  | Neq f f               -- ^ Inequality
  | LT f f                -- ^ Less Than
  | LTE f f               -- ^ Less Than or Equal To
  | GT f f                -- ^ Greater Than
  | GTE f f               -- ^ Greater Than or Equal To
  | Not f                 -- ^ Boolean Not
  | Xor f f               -- ^ Boolean Xor
  | And f f               -- ^ Boolean And
  | Or f f                -- ^ Boolean Or
  | Implies f f           -- ^ Logical Implication
  | Equals f f            -- ^ Logical Equality
  | All [f]               -- ^ Are all the elements of the list True?
  | Any [f]               -- ^ Are any of the elements of the list True?
  | Count [f]             -- ^ How many elements in the list are True?
  | Max [f]               -- ^ What is the value of the largest element in the list?
  | Min [f]               -- ^ What is the value of the smallest element in the list?
  | IsNamed f ExprName    -- ^ Attaches a name to an expression
  | WithComment f Comment -- ^ Attaches a comment to an expression
  deriving (Show, Read, Functor, Foldable, Traversable)

-- | Constraint Expression
--
--   In order to turn 'CExprF' into a traditional AST we take its fixed point
--   (i.e. something equivalent to @CExprF (CExprF (CExprF ( ...@).
--   In Haskell we do that using the type:
--   > newtype Fix f = Fix (f (Fix f))
--
--   This means that, as long as we unwrap the 'Fix' constructor repeatedly,
--   we can have infinitely nested 'CExprF's.
--
--   If that unwrapping seems onerous don't worry, we're going to use tools
--   from the 'recursion-schemes' package to automate much of it.
--
--   That said, we may also be wrapping our 'CExpr's in other types, to
--   add things like free variables and what

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
