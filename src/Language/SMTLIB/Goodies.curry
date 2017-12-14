--- ----------------------------------------------------------------------------
--- This module provides some goodies and utility functions for SMT-LIB.
---
--- @author  Jan Tikovsky
--- @version December 2017
--- ----------------------------------------------------------------------------
module Language.SMTLIB.Goodies where

import Language.SMTLIB.Types

infixl 7 *%
infixl 6 +%, -%
infix  4 =%, /=%, <%, >%, <=%, >=%

--- Declare a list of variables
declVars :: [(Symbol, Sort)] -> [Command]
declVars = map (uncurry DeclareConst)

--------------------------------------------------------------------------------
-- Smart constructors for SMT terms
--------------------------------------------------------------------------------

--- Construct SMT-LIB term from given integer
tint :: Int -> Term
tint = TConst . Num

--- Construct SMT-LIB term from given float
tfloat :: Float -> Term
tfloat = TConst . Dec

--- Construct SMT-LIB term from given character
tchar :: Char -> Term
tchar = TConst . Str . (: [])

--- Construct SMT-LIB term from given variable index
tvar :: Int -> Term
tvar vi = tcomb (var2SMT vi) []

--- Construct SMT-LIB term from a string
var :: String -> Term
var str = tcomb str []

--- Construct SMT-LIB constructor term
tcomb :: Ident -> [Term] -> Term
tcomb i ts = TComb (Id i) ts

--- Construct qualified SMT-LIB constructor term
qtcomb :: QIdent -> [Term] -> Term
qtcomb qi ts = TComb qi ts

--- Construct universally quantified SMT-LIB term
forAll :: [Int] -> [Sort] -> Term -> Term
forAll vs ss t = case vs of
  [] -> t
  _  -> Forall (zipWith SV (map var2SMT vs) ss) t

--- Negate given SMT-LIB term
tneg :: Term -> Term
tneg t = tcomb "not" [t]

--- Add two SMT-LIB terms
(+%) :: Term -> Term -> Term
t1 +% t2 = tcomb "+" [t1, t2]

--- Subtract an SMT-LIB term from another one
(-%) :: Term -> Term -> Term
t1 -% t2 = tcomb "-" [t1, t2]

--- Multiply two SMT-LIB terms
(*%) :: Term -> Term -> Term
t1 *% t2 = tcomb "*" [t1, t2]

--- Divide an SMT-LIB term by another one
(/%) :: Term -> Term -> Term
t1 /% t2 = tcomb "/" [t1, t2]

--- Constrain two SMT-LIB terms to be equal
(=%) :: Term -> Term -> Term
t1 =% t2 = tcomb "=" [t1, t2]

--- Constrain two SMT-LIB terms to be different
(/=%) :: Term -> Term -> Term
t1 /=% t2 = tcomb "not" [tcomb "=" [t1, t2]]

--- Constrain two SMT-LIB terms with a less-than-relation
(<%) :: Term -> Term -> Term
t1 <% t2 = tcomb "<" [t1, t2]

--- Constrain two SMT-LIB terms with a less-than-or-equal-relation
(<=%) :: Term -> Term -> Term
t1 <=% t2 = tcomb "<=" [t1, t2]

--- Constrain two SMT-LIB terms with a greater-than-relation
(>%) :: Term -> Term -> Term
t1 >% t2 = tcomb ">" [t1, t2]

--- Constrain two SMT-LIB terms with a greater-than-or-equal-relation
(>=%) :: Term -> Term -> Term
t1 >=% t2 = tcomb ">=" [t1, t2]

--- Combine a list of SMT-LIB terms using a conjunction
tand :: [Term] -> Term
tand = tcomb "and"

--- Constrain an SMT variable to be distinct from the given SMT constructors
noneOf :: Int -> Int -> [(QIdent, [Sort])] -> [Term]
noneOf idx dv qis = snd $ foldr ineq (idx, []) qis
  where ineq (qi, ss) (vi, cs) =
          let vn = vi - length ss
              vs = [vi, vi - 1 .. vn + 1]
          in (vn, forAll vs ss (tvar dv /=% qtcomb qi (map tvar vs)) : cs)

--------------------------------------------------------------------------------
-- Smart constructors for SMT sorts
--------------------------------------------------------------------------------

--- Construct an SMT-LIB sort
scomb :: Ident -> [Sort] -> Sort
scomb i ss = SComb i ss

--- Representation of 'Ordering' type as SMT-LIB sort
orderingSort :: Sort
orderingSort = scomb "Ordering" []

--- Representation of 'Int' type as SMT-LIB sort
intSort :: Sort
intSort = scomb "Int" []

--- Representation of '->' type constructor as SMT-LIB sort constructor
funSC :: [Sort] -> Sort
funSC = scomb "Fun"

--- Generate a `nop` SMT-LIB command
nop :: Command
nop = echo ""

--- Generate an `assert` SMT-LIB command
assert :: [Term] -> Command
assert ts = case ts of
  []  -> nop
  [t] -> Assert t
  _   -> Assert $ tand ts

--- Get the unqualified identifier of a qualified SMT-LIB identifier
unqual :: QIdent -> Ident
unqual (Id   i) = i
unqual (As i _) = i

--- Is given SMT-LIB command a declaration of an algebraic data type
isDeclData :: Command -> Bool
isDeclData cmd = case cmd of
  DeclareDatatype _ _ -> True
  DeclareDatatypes  _ -> True
  _                   -> False

--- Is given SMT-LIB command an 'Echo'
isEcho :: Command -> Bool
isEcho cmd = case cmd of
  Echo _ -> True
  _      -> False

--- Smart constructor for the 'Echo' SMT-LIB command
--- marking every 'Echo' command with an initial '@' character
--- which is necessary to recognize 'Echo's during parsing
echo :: String -> Command
echo str = Echo ('@' : str)

--- Smart constructor to generate a comment in an SMT-LIB script
comment :: String -> Command
comment = Comment

--- Transform a FlatCurry variable index into an SMT-LIB symbol
var2SMT :: Int -> Symbol
var2SMT vi = 'x' : show (abs vi)
