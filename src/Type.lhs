%
% Copyright (C) 1997 Thomas Nordin and Alastair Reid
%

\begin{code}
module Type
    ( 
      Type(..)
    , ppType

    , typeApply
    , typeArgs
    , typeResult
    , isNonIOType
    ) where

import Name

import Text.PrettyPrint
import PrettyUtils( ppTuple, ppParen )

\end{code}

@Type@ is used by the automatic fill-in to deduce
the @%call@ and @%result@ if they're missing from
a procedure specification.

\begin{code}
data Type       
  = Arrow Type Type
  | TypeList  Type
  | TypeTuple [Type]
  | TypeApply Type [Type]     -- non-empty arglist
  | TypeVar Name (Maybe Name) -- type name (and perhaps module where it is coming from).
  deriving ( Show )

typeApply :: Type -> [Type] -> Type
typeApply f []   = f
typeApply f args = TypeApply f args
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Pretty-printing types}
%*                                                                      *
%************************************************************************

\begin{code}
ppType :: Type -> Doc
ppType = go top_prec
 where
  go :: Int{-prec-} -> Type -> Doc
  go p (Arrow a b)	   = mbParen p fun_prec (go fun_prec a <+> text "->" <+> go p b)
  go _ (TypeList t)        = brackets (go top_prec t)
  go _ (TypeTuple ts)      = ppTuple (map (go top_prec) ts)
  go p (TypeApply f ts)    = mbParen p tycon_prec (hsep $ map (go tycon_prec) (f:ts))
  go _ (TypeVar s Nothing) = text s
  go _ (TypeVar s (Just m)) = text m <> char '.' <> text s

  mbParen context new_prec = ppParen (context >= new_prec)

  top_prec   = (0::Int)
  fun_prec   = (1::Int)
  tycon_prec = (2::Int)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Separating the arguments from the result in a type}
%*                                                                      *
%************************************************************************

\begin{code}
typeResult :: Type -> Type
typeResult (Arrow _ t2) = typeResult t2
typeResult x            = x

typeArgs :: Type -> [Type]
typeArgs (Arrow t1 t2) = t1 : typeArgs t2
typeArgs _             = []

isNonIOType :: Type -> Bool
isNonIOType t = case typeResult t of 
               TypeApply (TypeVar "IO" _) _ -> False
               _ -> True
\end{code}

