
The `language' being passed between the lexer and parser.

\begin{code}
module GCToken where

import Casm ( Kind )

data Token
 = T_fun
 | T_call
 | T_result
 | T_fail
 | T_const
 | T_enum
 | T_hinclude String
 | T_dllname
 | T_callconv
 | T_dis
 | T_prefix
 | T_oparen
 | T_cparen
 | T_ccurly
 | T_oangle
 | T_cangle
 | T_odangle
 | T_cdangle
 | T_osquare
 | T_csquare
 | T_comma
 | T_dot
 | T_dquote
 | T_equal
 | T_arrow
 | T_darrow
 | T_dcolon
 | T_declare
 | T_in
 | T_name     String
 | T_reccon   String
 | T_disname  String
 | T_kind     Kind
 | T_haskell  String
 | T_c        String
 | T_cexp     String
 | T_ccode    String
 | T_safecode  String
 | T_end       String
 | T_user      (String, String, Maybe Kind)
 | T_unknown   String
 | T_eof
   deriving (Show)

\end{code}
