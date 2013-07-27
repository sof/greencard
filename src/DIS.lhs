%
% Copyright (C) 1997 Thomas Nordin and Alastair Reid
%

\begin{code}
module DIS
    ( DIS(..)
    , apply
    , ppDIS, ppDIS'
    , expandDIS, DISEnv
    , freeVarsOfDIS
    , simplify

    , default_stdDIS
    ) where


import Casm( Kind(..), ppKind )
import Name( Name )
import ErrMonad

import Text.PrettyPrint
import PrettyUtils( ppTuple, commaList, textline )

import Data.Char( isAlphaNum, isLower )
import ListUtils ( equalLength )


\end{code}

%************************************************************************
%*                                                                      *
\subsection{DIS data structure}
%*                                                                      *
%************************************************************************

\begin{code}

data DIS
  = Var Name
  | CCode String
  | Kind Kind
    {-
     DIS application.
     Invariant: the argument list is never empty.
    -}
  | Apply DIS [DIS]
    {-
     Invariant: the field list is never empty.
    -}
  | Record Name [Name]
  | Constructor Name
     {-
      Invariant : declared DIS can only be a Var or CCode
     -}
  | Declare String DIS
  | Tuple
  | UserDIS Bool         -- are the marshall/unmarshall expressions IO actions?
            (Maybe Kind) -- the primitive type the marshall expression maps to.
            String       -- marshall (+args)
            String       -- unmarshall (+args)

instance Show DIS where
  showsPrec p d = showsPrec p (ppDIS d)

\end{code}

Always use this constructor to maintain the invariant that the
args part of an apply is non-empty.

\begin{code}

apply :: DIS -> [DIS] -> DIS
apply f [] = f
apply f as = Apply f as

\end{code}

%************************************************************************
%*                                                                      *
\subsection{Pretty Printing of DISs}
%*                                                                      *
%************************************************************************

\begin{code}

ppDIS :: DIS -> Doc
ppDIS = ppDIS' False

-- ppDIS' Can either print the type casts or not.

ppDIS' :: Bool -> DIS -> Doc
ppDIS' _ dis = pp dis
 where

  pp (Apply Tuple ds)            = ppTuple (pps ds)
  pp (Apply (Record name fs) ds) = text name <+> braces (commaList fields)
   where
    fields = zipWith (\n d -> textline [n, "="] <+> d) fs (pps ds)
  pp (Apply (Declare expr nm) [d]) = text ("(declare { "   ++
                                                    expr ++
						  " } ") <>
						  pp nm  <>
						  text  " in "  <>
						  pp d <>
						  char ')'
  pp (Apply d ds)                = parens (pp d <+> hsep (pps ds))
  pp (Record _ _)    		 = text "<record>"
  pp (Constructor nm)  		 = text nm
  pp Tuple             		 = text "()" -- unit
  pp (CCode s)         		 = braces (text s)
  pp (Declare ctype cv)     = text ("declare { " ++ ctype ++ " }") <+> pp cv
  pp (UserDIS io k ma unma) =
        (if io then text "<<" else char '<') <>
	   text ma   <+>
        char '/' <>
	   text unma <+>
        (case k of
	  Nothing -> empty
	  Just v  -> char '/' <+> ppKind v) <>
	(if io then text ">>" else char '>')
  pp (Var nm)      		 = text nm
  pp (Kind k)          		 = ppKind k

  pps = map pp

\end{code}

%************************************************************************
%*                                                                      *
\subsection{Free Variables}
%*                                                                      *
%************************************************************************

\begin{code}

freeVarsOfDIS :: DIS -> [Name]
freeVarsOfDIS = free
 where
  free (Apply d ds)   = free d ++ concatMap free ds
  free (Var nm)       = [nm]
  free (Declare  _ d) = free d
  free _              = []

\end{code}

%************************************************************************
%*                                                                      *
\subsection{Expanding DISs}
%*                                                                      *
%************************************************************************

Expanding a DIS is rather like evaluating an expression: we walk over
the DIS with an environment replacing disnames and arguments with
values from the environment. The result is a DIS in normal form.

\begin{code}

type DISEnv = [(Name, ([Name], DIS))]
type ArgEnv = [(Name, DIS)]

expandDIS :: DISEnv -> DIS -> ErrM String DIS
expandDIS denv dis = expandDIS' [] dis
 where
  expandDIS' :: ArgEnv -> DIS -> ErrM String DIS
  expandDIS' aenv di = xp di
   where

    xp :: DIS -> ErrM String DIS
    xp (Apply f@(UserDIS _ _ _ _) ds) = do
        ds' <- mapM xp ds
        f'  <- xp f
        return (Apply f' ds')

    xp r@(Apply f@(Var nm) ds) =
        case (lookup nm denv) of
          Just (args, d)
	    | equalLength args ds -> do
                   ds' <- mapM xp ds
	           expandDIS' (zip args ds') d
	    | otherwise ->
	        failure
		 (show $
		  text "" $$
		  hang (text "")
		   8   (hang (text "DIS application" <+> quotes (ppDIS r) <+>
			      text "incompatible with definition:")
			 4  (text "%dis" <+> hsep (map text (nm:args)) <+>
			     equals <+> ppDIS d)))
          Nothing -> do
             ds' <- mapM xp ds
	     return (Apply f ds')

    xp (Apply d ds) = do
        d'  <- xp d
	ds' <- mapM xp ds
        return (Apply d' ds')

    xp v@(Var nm) =
        case (lookup nm aenv) of
          Just d   -> return d
          Nothing  -> return v

    xp (Declare ctype cv) = do
        ctype' <- subst ctype
        cv'    <- xp cv
        return (Declare ctype' cv')

    xp (CCode ccode) = do
        ccode' <- subst ccode
        return (CCode ccode')

    xp (UserDIS io k ma unma) = do
        ma'   <- subst ma
        unma' <- subst unma
        return (UserDIS io k ma' unma')

    -- everything else is already in normal form
    xp d = return d

    -- substitute for anything of the form %[a-z][a-zA-Z0-9]*
    subst :: String -> ErrM String String
    subst ""           = return ""
    subst ('%':'%':cs) = do  -- escape code
        cs' <- subst cs
	return ('%' : cs')
    subst ('%':c:cs) | isLower c =
        case lookup nm aenv of
          Just (CCode c1) -> do
	       rest' <- subst rest
	       return (c1 ++ rest')
          Just (Var   v) -> do
	       rest' <- subst rest
	       return (v ++ rest')
          Just d'        -> do
	       failure (unwords
	       		   [ "Can't substitute"
			   , show (ppDIS d')
			   , "for"
			   , nm
			   , "in DIS"
			   , show (ppDIS di)
			   ])
          Nothing -> do
	       failure (unwords
	       		   [ "Unknown variable"
	       		   , nm
			   , "in DIS"
			   , show (ppDIS di)
			   ])
     where
      (cs', rest) = span isAlphaNum cs
      nm = c:cs'

    subst (c:cs)  = do
      cs' <- subst cs
      return (c:cs')
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Simplify DISs}
%*                                                                      *
%************************************************************************

Simplify a DIS by pushing casts down to the leaves

\begin{code}
simplify :: DIS -> DIS
simplify (Apply (CCode _) [dis@(Apply (CCode _) _)]) = simplify dis
simplify (Apply f as) = Apply (simplify f) (map simplify as)
simplify dis          = dis
\end{code}

Well and truly stable by now, so don't mind too much duplicating this:

\begin{code}
default_stdDIS :: String
default_stdDIS = unlines [
     "%dis char x             = %%Char   ({HsChar}   x)"
   , "%dis int x              = %%Int    ({HsInt}    x)"
   , "%dis float x            = %%Float  ({HsFloat}  x)"
   , "%dis double x           = %%Double ({HsDouble} x)"
   , ""
   , "%dis ptr x              = %%Ptr    ({HsPtr}    x)"
   , "%dis funPtr x           = %%FunPtr ({HsFunPtr} x)"
   , ""
   , "%dis int8 x             = %%Int8   ({HsInt8}   x)"
   , "%dis int16 x            = %%Int16  ({HsInt16}  x)"
   , "%dis int32 x            = %%Int32  ({HsInt32}  x)"
   , ""
   , "%dis word8 x            = %%Word8  ({HsWord8}  x)"
   , "%dis word16 x           = %%Word16 ({HsWord16} x)"
   , "%dis word32 x           = %%Word32 ({HsWord32} x)"
   , ""
   , "%dis maybeT z x         = %Maybe z x"
   , "%dis maybe x            = maybeT {0} x"
   , ""
   , "%dis bool x             = bool_ (int x)"
   , ""
   , "%dis iO x = x"
   , "%dis string x      = string_    (ptr ({char *} x))"
   , "%dis stringLen x l = stringLen_ (ptr ({char *} x)) (int l)"
   , ""
   , "%dis mbString x    = maybeT {nullPtr} (string x)"
   , ""
   , "%dis cString x      = ptr ({char *} x)"
   , "%dis cStringLen x l = (ptr ({char *} x), int l)"
   , ""
   , "%dis stable x = "
   , "%   declare {HsStablePtr} x in"
   , "%   << makeStablePtr / deRefStablePtr / %%StablePtr >> x"
   , ""
   , "%dis stablePtr x = (%%StablePtr ({HsStablePtr} x))"
   , "%dis foreignPtr finalizer x = %ForeignPtr {HsPtr} x finalizer"
   , ""
   , "%dis foreignP y = foreignPtr {free} y"
   ]

\end{code}

%************************************************************************
%*                                                                      *
\subsection{Example DISs}
%*                                                                      *
%************************************************************************

COMMENTED OUT:

\begin{code}
{-
dis1 = Apply (Kind Int)   [ CCode "int",   Var "x" ]
dis2 = Apply (Kind Float) [ CCode "float", Var "y" ]
dis3 = Apply Tuple [dis1,dis2]

disenv1 =
  [ ( "int",   (["x"], dis1) )
  , ( "float", (["y"], dis2) )
  ]

dis4 = Apply (Var "int") [Var "arg1"]
dis5 = expandDIS disenv1 dis4
-}
\end{code}


