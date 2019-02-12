
{-|
Module      : Types.CExpr
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

-}

module Types.CExpr where

import Data.Functor.Foldable (Fix)
import Data.Functor.Foldable.TH

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
-- Here we define the AST's type '

-- * Expression Name

-- | We use a type synonym/type alias to say that (for now) we are using
--   'String's as names for types. In the future you may wish to change it
--   and when you do, the compiler will complain about the generated errors,
--   making it easy to find and fix them.
type ExprName = String

-- * Comment

-- |  We use a type synonym/type alias to say that (for now) we are using
--   'String's to store comments.
type Comment = String

-- * Constraint Expression

-- | This is the normal recursive datatype we use to represent constraint
--   expressions.
--
--   The type parameter 'l' indicates the type we use for literals. If you
--   want all literals to be integers, you use @CExpr Int@. Likewise for
--   anything else.
--
--   Mind, literals do /not/ have to be constants, they could be free variables
--   a reference to bound terms, or the like.
--
--   You can use the constructors to generate values of this type, and
--   pattern matching to extract them.
data CExpr l
  = Add         (CExpr l) (CExpr l) -- ^ Addition
  | Sub         (CExpr l) (CExpr l) -- ^ Subtraction
  | Negate      (CExpr l)           -- ^ Unary Negation
  | Mult        l         (CExpr l) -- ^ Multiplication: one of the terms is a literal
                                    --   because we are restricting the output language
                                    --   to linear constraints. Making sure that
                                    --   one value in every Mult op is a constant is a
                                    --   good way to do that.
  | Sum         [CExpr l]           -- ^ Sum of all elements in a list
  | Eq          (CExpr l) (CExpr l) -- ^ Equality
  | Neq         (CExpr l) (CExpr l) -- ^ Inequality
  | LT          (CExpr l) (CExpr l) -- ^ Less Than
  | LTE         (CExpr l) (CExpr l) -- ^ Less Than or Equal To
  | GT          (CExpr l) (CExpr l) -- ^ Greater Than
  | GTE         (CExpr l) (CExpr l) -- ^ Greater Than or Equal To
  | Not         (CExpr l)           -- ^ Boolean Not
  | Xor         (CExpr l) (CExpr l) -- ^ Boolean Xor
  | And         (CExpr l) (CExpr l) -- ^ Boolean And
  | Or          (CExpr l) (CExpr l) -- ^ Boolean Or
  | Implies     (CExpr l) (CExpr l) -- ^ Logical Implication
  | Equals      (CExpr l) (CExpr l) -- ^ Logical Equality
  | All         [CExpr l]           -- ^ Are all the elements of the list True?
  | Any         [CExpr l]           -- ^ Are any of the elements in the list True?
  | Count       [CExpr l]           -- ^ How many elements in the list are True?
  | Max         [CExpr l]           -- ^ What is the value of the largest element in the list?
  | Min         [CExpr l]           -- ^ What is the value of the smallest element in the list?
  | Lit         l                   -- ^ This stores a value of type 'a'
  | IsNamed     (CExpr l) ExprName  -- ^ Attaches a name to an expression
  | WithComment (CExpr l) Comment   -- ^ Attaches a comment to an expression
  deriving (Show, Read, Functor, Foldable, Traversable)

-- * Constraint Expression Functor

-- | We use a tool from the 'recursion-schemes' package to generate a functor
--   version of 'CExpr'. This type replaces every nested instance of a 'CExpr'
--   in the type with 'f', a new type parameter.
--
--   In order to turn 'CExprF' into a traditional AST we take its fixed point
--   (i.e. something equivalent to @CExprF l (CExprF l (CExprF l ( ...@).
--   In Haskell we do that using the type:
--   > newtype Fix f = Fix (f (Fix f))
--
--   This means that, as long as we unwrap the 'Fix' constructor repeatedly,
--   we can have infinitely nested 'CExprF's.
--
--   If that unwrapping seems onerous don't worry, we're going to use tools
--   from the 'recursion-schemes' package to automate much of it.
makeBaseFunctor ''CExpr

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
