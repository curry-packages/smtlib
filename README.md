smtlib: Implementation of the SMT-LIB standard for Curry
========================================================

SMT-LIB is a common standard for interacting with SMT solvers.
This package contains libraries to deal with SMT-LIB scripts, i.e., the input
and output language of SMT solvers.
The libraries provided by this package implement Version 2.6 of the SMT-LIB
Standard (http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.6-r2017-07-18.pdf).

The package currently includes:

* `Language.SMTLIB.Files`: This module provides file operations for SMT-LIB
  scripts.
* `Language.SMTLIB.Goodies`: This module provides smart constructors and some
  useful auxiliary functions for SMT-LIB data terms.
* `Language.SMTLIB.Parser`: This module provides a parser generating SMT-LIB
  data terms from string representations of SMT-LIB scripts. Currently, there is
  only support for parsing a subset of SMT-LIB, namely command responses.
* `Language.SMTLIB.Pretty`: This module provides a pretty-printers for abstract
  representations of SMT-LIB scripts and all substructures.
* `Language.SMTLIB.Scanner`: This module provides a simple scanner for
  SMT-LIB scripts.
* `Language.SMTLIB.Types`: This module defines data types to represent SMT-LIB
  scripts in Curry.

* `Language.SMTLIB`: This module bundles most of the functionality provided by
  the specific libraries mentioned above.

* `ParserComb`: This module provides some common parser combinators which are
  used in the implementation of the SMT-LIB parser.
  In the future, this module should be moved to a separate package.
