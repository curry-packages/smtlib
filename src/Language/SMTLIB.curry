--- ----------------------------------------------------------------------------
--- Implementation of the SMT-LIB language for Curry
---
--- The implementation is based on the SMT-LIB Standard 2.6
--- (http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.6-r2017-07-18.pdf)
--- and covers most parts of the language description.
---
--- @author  Jan Tikovsky
--- @version December 2017
--- ----------------------------------------------------------------------------
module Language.SMTLIB
  ( module Language.SMTLIB.Files
  , module Language.SMTLIB.Goodies
  , module Language.SMTLIB.Pretty
  , module Language.SMTLIB.Types
  , SMTParser, parseCmdRsps
  ) where

import Language.SMTLIB.Files
import Language.SMTLIB.Goodies
import Language.SMTLIB.Parser
import Language.SMTLIB.Pretty
import Language.SMTLIB.Scanner
import Language.SMTLIB.Types   hiding (Echo)
