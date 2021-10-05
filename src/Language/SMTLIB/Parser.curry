--- ----------------------------------------------------------------------------
--- This module provides parser functions for the SMT-LIB language.
--- Currently there are only parsers for a subset of the language,
--- namely command responses.
---
--- @author  Jan Tikovsky (with changes by Niels Bunkenburg)
--- @version August 2021
--- ----------------------------------------------------------------------------
module Language.SMTLIB.Parser where

import           ParserComb
import           Language.SMTLIB.Scanner
import qualified Language.SMTLIB.Types   as SMT

type SMTParser a = Parser Token a

parseCmdRsps :: String -> Either String [SMT.CmdResponse]
parseCmdRsps str = case (runParser (many parseResponse) . scan) str of
  Left  msg        -> Left msg
  Right ([], rsps) -> Right rsps
  Right (ts,    _) -> Left $ "Incomplete parse: " ++ show ts

parseResponse :: SMTParser SMT.CmdResponse
parseResponse =  terminal KW_success     *> return SMT.SuccessRsp
             <|> terminal KW_sat         *> return (SMT.CheckSatRsp SMT.Sat)
             <|> terminal KW_unsat       *> return (SMT.CheckSatRsp SMT.Unsat)
             <|> terminal KW_unknown     *> return (SMT.CheckSatRsp SMT.Unknown)
             <|> terminal KW_unsupported *> return SMT.UnsupportedRsp
             <|> terminal LParen *> parseErrorRsp         <* terminal RParen
             <|> terminal LParen *> parseGetValueRsp      <* terminal RParen
             <|> terminal LParen *> parseGetModelRsp      <* terminal RParen
             <|> terminal LParen *> parseGetAssertionsRsp <* terminal RParen
             <|> terminal LParen *> parseGetAssignmentRsp <* terminal RParen
             <|> terminal LParen *> parseGetInfoRsp       <* terminal RParen
             <|> return SMT.EchoRsp <*> parseStr
             <|> return SMT.GetOptionRsp <*> parseAttrValue
             <|> return SMT.GetProofRsp <*> parseSExpr
             -- the get-unsat-assumptions-response and the get-unsat-core-response overlap
--              <|> return SMT.GetUnsatAssumptionsRsp <*> many parseSym
--              <|> return SMT.GetUnsatCoreRsp <*> many parseSym

--- parser for get-assertions-response
parseGetAssertionsRsp :: SMTParser SMT.CmdResponse
parseGetAssertionsRsp = return SMT.GetAssertionsRsp <*> many parseTerm

--- parser for get-assignment-response
parseGetAssignmentRsp :: SMTParser SMT.CmdResponse
parseGetAssignmentRsp = return SMT.GetAssignmentRsp <*> many parseTValPair

--- parser for a t-valuation pair
parseTValPair :: SMTParser (SMT.Symbol, Bool)
parseTValPair =
  terminal LParen *> return (,) <*> parseSym <*> parseBool <* terminal RParen

--- parser for get-info-response
parseGetInfoRsp :: SMTParser SMT.CmdResponse
parseGetInfoRsp = return SMT.GetInfoRsp <*> some parseInfoRsp

--- parser for an info response
parseInfoRsp :: SMTParser SMT.InfoRsp
parseInfoRsp
  =  terminal Colon *> terminal KW_assertion_stack_levels
                    *> return SMT.AssertionStackLevelsRsp <*> parseNum
 <|> terminal Colon *> terminal KW_authors *> return SMT.AuthorsRsp <*> parseStr
 <|> terminal Colon *> terminal KW_error_behavior *> return SMT.ErrorBehaviorRsp
                   <*> parseErrorBehavior
 <|> terminal Colon *> terminal KW_name *> return SMT.NameRsp <*> parseStr
 <|> terminal Colon *> terminal KW_reason_unknown *> return SMT.ReasonUnknownRsp
                   <*> parseReasonUnknown
 <|> terminal Colon *> terminal KW_version *> return SMT.VersionRsp <*> parseStr
 <|> return SMT.AttrRsp <*> parseAttribute

--- parser for error behavior
parseErrorBehavior :: SMTParser SMT.ErrorBehavior
parseErrorBehavior
  =  terminal KW_immediate_exit *> return SMT.ImmediateExit
 <|> terminal KW_continued_execution *> return SMT.ContinuedExecution

--- parser for reason unknown
parseReasonUnknown :: SMTParser SMT.ReasonUnknown
parseReasonUnknown =  terminal KW_memout *> return SMT.Memout
                  <|> terminal KW_incomplete *> return SMT.Incomplete
                  <|> return SMT.SEReason <*> parseSExpr

--- parser for a get-model-response
parseGetModelRsp :: SMTParser SMT.CmdResponse
parseGetModelRsp =
  -- Note: the keyword 'model' is not part of the SMTLib standard (but used in Z3)
  terminal KW_model *> return SMT.GetModelRsp <*> many parseModelRsp

--- parser for a model response
parseModelRsp :: SMTParser SMT.ModelRsp
parseModelRsp
  =  terminal LParen *> terminal KW_def_fun *> return SMT.MRFun <*> parseFunDef
                    <*  terminal RParen
 <|> terminal LParen *> terminal KW_def_fun_rec *> return SMT.MRFunRec
                    <*> parseFunDef <* terminal RParen
 <|> terminal LParen *> terminal KW_def_funs_rec
                     *> return ((SMT.MRFunsRec .) . zip) <*  terminal LParen
                    <*> some parseFunDec <* terminal RParen
                    <*  terminal LParen <*> some parseTerm   <* terminal RParen
                    <*  terminal RParen

--- parser for a function definition
parseFunDef :: SMTParser SMT.FunDef
parseFunDef = return SMT.FunDef <*> parseSym <* terminal LParen
  <*> many parseSortedVar <* terminal RParen <*> parseSort <*> parseTerm

--- parser for a function declaration
parseFunDec :: SMTParser SMT.FunDec
parseFunDec
  = terminal LParen *> return SMT.FunDec <*> parseSym <* terminal LParen
 <*> many parseSortedVar <* terminal RParen <*> parseSort <* terminal RParen

--- parser for a get-value-response
parseGetValueRsp :: SMTParser SMT.CmdResponse
parseGetValueRsp = return SMT.GetValueRsp <*> some parseValPair

--- parser for an error response
parseErrorRsp :: SMTParser SMT.CmdResponse
parseErrorRsp =  terminal KW_error *> return SMT.ErrorRsp <*> parseStr

--- parser for a boolean value
parseBool :: SMTParser Bool
parseBool = Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  BVal b : ts -> Right (ts, b)
  t      : ts -> (runParser (unexpected t)) ts

--- parser for a numeral
parseNum :: SMTParser SMT.Numeral
parseNum = Parser $ \tokens -> case tokens of
  []         -> (runParser eof) tokens
  Num n : ts -> Right (ts, n)
  t     : ts -> (runParser (unexpected t)) ts

--- parser for a string
parseStr :: SMTParser String
parseStr = Parser $ \tokens -> case tokens of
  [] -> runParser eof tokens
  DQuote : ts -> let (toks, _:rs) = span (/= DQuote) ts
                 in Right (rs, unwords (map toStr toks))
  t      : ts -> runParser (unexpected t) ts
 where
  toStr tok = case tok of
    Num      n -> show n
    Id       s -> s
    BVal     b -> show b
    Colon      -> ":"
    Semi       -> ";"
    Comma      -> ","
    Bang       -> "!"
    Underscore -> "_"
    LParen     -> "("
    RParen     -> ")"
    t          -> show t

--- parser for a symbol
parseSym :: SMTParser SMT.Symbol
parseSym = Parser $ \tokens -> case tokens of
  []        -> (runParser eof) tokens
  Id s : ts -> Right (ts, s)
  t    : ts -> (runParser (unexpected t)) ts

--- parser for a valuation pair
parseValPair :: SMTParser SMT.ValuationPair
parseValPair =
  terminal LParen *> return (,) <*> parseTerm <*> parseTerm <* terminal RParen

--- parser for terms
parseTerm :: SMTParser SMT.Term
parseTerm = SMT.TConst . SMT.Str <$> parseStr <|> (Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  Num  n : ts -> Right (ts, SMT.TConst (SMT.Num n))
  BVal b : ts -> let s = case b of
                           True  -> "true"
                           False -> "false"
                 in Right (ts, SMT.TComb (SMT.Id s) [])
  Id   s : ts -> Right (ts, SMT.TComb (SMT.Id s) [])
  LParen : ts -> (runParser parseParenTerm) ts
  t      : ts -> (runParser (unexpected t)) ts)

--- parser for parenthesized terms
parseParenTerm :: SMTParser SMT.Term
parseParenTerm
     -- parser for negative numbers
  =  return (SMT.TConst . SMT.Num) <*> parseNum <* terminal RParen
     -- parser for qualified identifiers
 <|> terminal KW_as *> return SMT.TComb <*> (return SMT.As <*> parseSym
                   <*> parseSort) <*> return [] <* terminal RParen
     -- parser for complex terms
 <|> return SMT.TComb <*> parseQIdent <*> some parseTerm <* terminal RParen
     -- parser for let terms
 <|> terminal KW_let *> return SMT.Let <* terminal LParen <*> some parseVarBind
                    <* terminal RParen <*> parseTerm <* terminal RParen
     -- parser for universally quantified terms
 <|> terminal KW_forall *> return SMT.Forall <* terminal LParen
                       <*> some parseSortedVar <* terminal RParen
                       <*> parseTerm <* terminal RParen
     -- parser for existentially quantified terms
 <|> terminal KW_exists *> return SMT.Exists <* terminal LParen
                       <*> some parseSortedVar <* terminal RParen
                       <*> parseTerm <* terminal RParen
     -- parser for annotated terms
 <|> terminal Bang *> return SMT.Annot <*> parseTerm <*> some parseAttribute
                  <* terminal RParen

--- parser for let bindings
parseVarBind :: SMTParser (SMT.Symbol, SMT.Term)
parseVarBind =
  terminal LParen *> return (,) <*> parseSym <*> parseTerm <* terminal RParen

--- parser for sorted variables
parseSortedVar :: SMTParser SMT.SortedVar
parseSortedVar =
  terminal LParen *> return SMT.SV <*> parseSym <*> parseSort <* terminal RParen

--- parser for attributes
parseAttribute :: SMTParser SMT.Attribute
parseAttribute = Parser $ \tokens -> case tokens of
  []                             -> (runParser eof) tokens
  Colon : (Id s : (RParen : ts)) -> Right (RParen : ts, SMT.AKW (SMT.KW s))
  Colon : (Id s           : ts)  -> (runParser (return (SMT.AVal (SMT.KW s))
                                      <*> parseAttrValue)) ts
  t                       : ts   -> (runParser (unexpected t)) ts

--- parser for an attribute value
parseAttrValue :: SMTParser SMT.AttrValue
parseAttrValue = Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  Num n  : ts -> Right (ts, SMT.AVConst (SMT.Num n))
  Id  s  : ts -> Right (ts, SMT.AVSym s)
  LParen : ts -> (runParser (return SMT.AVSExpr <*> many parseSExpr
                   <* terminal RParen)) ts
  t      : ts -> (runParser (unexpected t)) ts

--- parser for `SExpr`
parseSExpr :: SMTParser SMT.SExpr
parseSExpr = Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  Num n  : ts -> Right (ts, SMT.SEConst (SMT.Num n))
  Id  s  : ts -> Right (ts, SMT.SESym s)
  Colon  : ts -> (runParser (return (SMT.SEKW . SMT.KW) <*> parseSym)) ts
  LParen : ts -> (runParser (return SMT.SEList <*> many parseSExpr
                   <* terminal RParen)) ts
  t      : ts -> (runParser (unexpected t)) ts

--- parser for qualified identifiers
parseQIdent :: SMTParser SMT.QIdent
parseQIdent = Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  Id  s  : ts -> Right (ts, SMT.Id s)
  LParen : ts -> (runParser (terminal KW_as *> return SMT.As <*> parseSym
                   <*> parseSort <* terminal RParen)) ts
  t      : ts -> (runParser (unexpected t)) ts

--- parser for sorts
parseSort :: SMTParser SMT.Sort
parseSort = Parser $ \tokens -> case tokens of
  []          -> (runParser eof) tokens
  Id s   : ts -> Right (ts, SMT.SComb s [])
  LParen : ts -> (runParser (return SMT.SComb <*> parseSym <*> some parseSort
                   <*  terminal RParen)) ts
  t      : ts -> (runParser (unexpected t)) ts
