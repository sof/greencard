% 
% Copyright (C) 1997, 2003 Thomas Nordin and Alastair Reid
%

> {
> module Parse
>        (
>         gcParse		-- :: [Token] -> [Decl]
>        ) where
> 
> import Lex
> import LexM
> import GCToken
> import Name( Name )
> import Type( Type(..), typeApply )
> import DIS ( DIS(..), apply )
> import Decl
> {-
> BEGIN_GHC_ONLY
> import GHC.Exts
> END_GHC_ONLY
> -}
> 
> }
> 
> %name gcParse
> %tokentype { Token }
> %lexer { topLex } { T_eof }
> %monad { LexM } { thenLexM } { returnLexM }
> 
> %token 
> 	'%fun'		{ T_fun         }
> 	'%call'		{ T_call        }
> 	'%result'	{ T_result      }
> 	'%fail'		{ T_fail        }
> 	'%const'	{ T_const       }
> 	'%enum'		{ T_enum        }
> 	'%#include'	{ T_hinclude $$ }
> 	'%dis'		{ T_dis         }
>       '%dllname'	{ T_dllname     } 
>       '%callconv'	{ T_callconv    } 
> 	'%prefix'	{ T_prefix      }
> 	'%C'		{ T_c $$        }
> 	'('		{ T_oparen      }
> 	')'		{ T_cparen      }
> 	'<<'		{ T_odangle     }
> 	'>'		{ T_cangle      }
> 	'<'		{ T_oangle      }
> 	'>>'		{ T_cdangle     }
> 	'}'		{ T_ccurly      }
> 	'['		{ T_osquare     }
> 	']'		{ T_csquare     }
> 	','		{ T_comma       }
> 	'.'		{ T_dot         }
> 	'='		{ T_equal       }
> 	'=>'		{ T_darrow      }
> 	'->'		{ T_arrow       }
> 	'::'		{ T_dcolon      }
>       '"'             { T_dquote      }
>       'declare'       { T_declare     }
>       'in'            { T_in          }
> 	name		{ T_name     $$ }
> 	recname		{ T_reccon   $$ }
> 	disname		{ T_disname  $$ }
>       kind            { T_kind     $$ }
>       user            { T_user     $$ }
> 	haskell		{ T_haskell  $$ }
> 	cexp		{ T_cexp     $$ }
> 	ccode		{ T_ccode    $$ }
> 	safeccode	{ T_safecode $$ }
> 	'%end'		{ T_end      $$ }
>       UNKNOWN         { T_unknown  $$ }
> 
> %%
> 
> Decls		:: { [Decl] }
> 		:				{ [] }
> 		| Decl Decls			{ $1 : $2 }
> 
> Decl		:: { Decl }
> 		: ProcSpec			{ $1 } 
> 		| haskell			{ Haskell $1 }
> 		| '%dis' disname Vars '=' DIS 	{ DisDef $2 $3 $5}
> 		| '%const' Type '[' ConstDefs ']' { Constant $2 $4 }
>		| '%enum' Name Derivings Name '[' ConstDefs ']' { Enum $2 (TypeVar $4 Nothing) $3 $6 }
> 		| '%prefix' Name		{ Prefix $2 }
>		| '%C'				{ C $1 }
> 		| '%#include' 			{ Include $1 }
>		| '%dllname' Name		{ DllName $2 }
>		| '%callconv' Name		{ CConv $2   }
> 
> 
> Vars		:: { [Name] }
> 		: 				{ [] }
> 		| disname Vars			{ $1 : $2 }
> 
> ConstDefs	:: { [(Name, Name)] }
>		: ConstDef			{ [$1] }
> 		| ConstDef ',' ConstDefs	{ $1 : $3 }
> 
> ConstDef     :: { (Name, Name) }
>               : Name                          { ($1, $1) }
>               | Name '=' Name			{ ($1, $3) }
>               | Name '=' cexp			{ ($1, $3) }
> 
> Derivings     :: { [Name] }
>                : {-empty-}                    { [] }
>		 | '(' NameList ')'		{ $2 }
> 
> NameList      :: { [Name] }
>                : {- empty -}			{ [] }
>		 | Names			{ $1 }
> 
> Names		:: { [Name] }
> 		: Name				{ [$1] }
> 		| Name ',' Names		{ $1:$3 }
> 
> Name		:: { Name }
> 		: name				{ $1 }
> 		| disname			{ $1 }
> 
> ProcSpec	:: { Decl }
> 		: Sig MbCall MbCCode MbFail MbResult MbEnd	{ ProcSpec $1 $2 $3 $4 $5 $6 }
> 
> Sig		:: { Sig }
> 		: '%fun' MbFun Name '::' HType	{% let (mb_ctxt, t) = $5 in getSrcLoc >>= \ sl -> return (setSrcLocName $3 sl, $3, $2, mb_ctxt, t) }
> 
> HType         :: { (Maybe Type, Type) }
> HType         : Type '=>' Type                { (Just $1, $3) }
>               | Type                          { (Nothing, $1) }
> 
> Type		:: { Type }
> 		: SimpleType	 		{ $1 }
> 		| SimpleType '->' Type 		{ Arrow $1 $3 }
> 
> SimpleType	:: { Type }
> 		: '(' Type ')' 			{ $2 }
> 		| TypeTuple			{ $1 }
> 		| Name '.' Name ATypes		{ typeApply (TypeVar $3 (Just $1)) $4 }
> 		| Name ATypes 			{ typeApply (TypeVar $1 Nothing) $2 }
>		| '[' Type ']'			{ TypeList $2 }
> 
> ATypes	:: { [Type] }
> 		:				{ [] }
> 		| AType ATypes			{ $1 : $2 }
> 
> AType		:: { Type }
> 		: '(' Type ')' 			{ $2 }
> 		| TypeTuple			{ $1 }
> 		| Name '.' Name			{ TypeVar $3 (Just $1) }
> 		| Name				{ TypeVar $1 Nothing  }
> 
> TypeTuple	:: { Type }
> 		: '(' ')'			{ TypeTuple [] }
> 		| '(' TypeTuples ')'		{ TypeTuple $2 }
> TypeTuples	:: { [Type] }
> 		: Type ',' Type			{ [$1, $3] }
> 		| Type ',' TypeTuples		{ $1 : $3 }
> 
> CCode		:: { String }
> 		: ccode				{ $1 }
> 		| cexp				{ $1 }
> 
> MbCall	:: { Maybe Call }
> 		:				{ Nothing }
> 		| '%call' Args			{ Just $2 }
> 
> MbEnd		:: { Maybe String }
> 		:				{ Nothing }
> 		| '%end'			{ Just $1 }
> 
> MbCCode	:: { Maybe CCode }
> 		: 				{ Nothing }
> 		| safeccode			{ Just (True,$1) }
> 		| ccode				{ Just (False,$1) }

> 
> MbFail	:: { Maybe Fail }
> 		:				{ Nothing }
> 		| Fails				{ Just $1 }
> 
> Fails		:: { [(String, String)] }
> 		: '%fail' CCode CCode		{ [($2, $3)] }
> 		| '%fail' CCode CCode Fails	{ ($2, $3) : $4 }
> 
> MbResult	:: { Maybe Result }
> 		:				{ Nothing }
> 		| '%result' DIS			{ Just $2 }
> 
> MbFun		:: { Maybe String }
> 		:				{ Nothing }
> 		| '"' Name '"'			{ Just $2 }
> 
> DIS		:: { DIS }
> 		: '(' DIS ')'			{ $2 }
> 		| disname Args			{ apply (Var $1) $2 }
> 		| name Args			{ apply (Constructor $1) $2 }
> 		| recname FieldDISs '}'	        { let (fs,ds) = unzip $2
>                                                 in Apply (Record $1 fs) ds }
>		| kind Arg			{ apply (Kind $1) [$2] }
>		| '<'  user '>'  Args   { let (marshall,unmarshall,kind) = $2 
>						  in apply (UserDIS False kind marshall unmarshall) $4 }
>		| '<<' user '>>' Args   { let (marshall,unmarshall,kind) = $2 
>						  in apply (UserDIS True kind marshall unmarshall) $4 }
> 		| Tuple				{ $1 }
> 		| cexp 				{ CCode $1 }
> 		| cexp DIS			{ apply (CCode $1) [$2] }
>		| 'declare' cexp Name 'in' DIS  { apply (Declare $2 (Var $3)) [$5] }
> 
> Arg		:: { DIS }
> 		: '(' DIS ')'			{ $2 }
> 		| disname			{ Var $1 }
> 		| name				{ Constructor $1 }
> 		| Tuple				{ $1 }
> 		| cexp				{ CCode $1 }
> 
> Args		:: { [DIS] }
> 		: 				{ [] }
> 		| Arg Args			{ $1 : $2 }
> 
> Tuple		:: { DIS }
> 		: '(' ')'			{ Tuple }
> 		| '(' Tuples ')'		{ apply Tuple $2 }
> 
> Tuples	:: { [DIS] }
> 		: DIS ',' DIS			{ [$1, $3] }
> 		| DIS ',' Tuples		{ $1 : $3 }
> 
> FieldDISs	:: { [(String, DIS)] }
> 		 : field_elt			{ [$1] }
> 		 | field_elt ',' FieldDISs	{ $1 : $3 }
> 
> field_elt	:: { (String, DIS) }
> 		: Name '=' DIS	 		{ ($1, $3) }
> 
> {
> 
> happyError :: LexM a
> happyError = do
>  l   <- getSrcLoc
>  str <- getStream
>  error (show l ++ ": Parse error: " ++ take 100 str)
> }
> 
