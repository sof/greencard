%
% Copyright (C) 1997 Thomas Nordin and Alastair Reid
%

Automatic filling in missing pieces of a Green Card declarations.

\begin{code}
module FillIn
	( 
	  fillInDecls   -- :: DISEnv -> [String] -> Target -> [Decl] -> ([String], [Decl])
        ) where

import Decl
import Target
import Name ( Name )
import Type ( Type(..), typeArgs, typeResult )
import DIS  ( DIS(..), DISEnv, apply, expandDIS )
import NameSupply
import ListUtils( mkHaskellVar, haskelliseName
		, lowerName
		)

import FillInMonad
import Data.Maybe( fromMaybe )
import Data.Char ( isLower )
import Data.List( intersperse )

\end{code}

The fill-in storefront is @fillInDecls@ which
just invokes the function that does the real job,
@fillIn@.

\begin{code}
fillInDecls :: DISEnv
	    -> [String]
	    -> Target
	    -> Bool
	    -> [Decl]
	    -> ([String], [Decl])
fillInDecls env pref trgt mangle decls =
 case (runFilM env pref trgt mangle (map fillIn decls)) of
   (errs, declss) -> (errs, concat declss)
\end{code}

The main entry point, @fillIn@, performs a pair of
tasks:

  - fills in missing parts of a procedure specification
    (%call, %result or %code)
  - expands/reduces the DISs used on the %call and %result
    lines.

\begin{code}
fillIn :: Decl -> FilM [Decl]
fillIn (ProcSpec (loc, nm, mbfun, mb_ctxt, ty) mbcall mbccode fs mbres mbend) = 
  addErrorContext (show loc ++ ": ") $ do
  env  <- getDISEnv
  -- Expand the DISes used on the %call and %result lines.
  let (auto_call, auto_res) = typeToDIS env ty
      xp dis                = liftErrM (expandDIS env dis)
  call <- mapM xp (fromMaybe auto_call mbcall)
  res  <- xp      (fromMaybe auto_res  mbres)
  let fun_nm = mangleFunName mbfun nm ty
  let ccode                 = fromMaybe (expandCCode fun_nm call res) mbccode
  prefixes <- getPrefixes
  mangle   <- getNameMangleFlag
  return [ProcSpec (loc, mkHaskellVar (haskelliseName mangle prefixes nm), Nothing, mb_ctxt, ty)
	           (Just call)
	           (Just ccode)
	           fs
	           (Just res)
	           mbend]

fillIn (Constant ty defs) = do
  tgt    <- getTarget
  pre    <- getPrefixes
  mangle <- getNameMangleFlag
  case tgt of
{- Sigh, the use of lit-lits will break if
   constants map to something that's not CCallable
   (e.g., "newtype Foo = MkFoo Int"). 
 
   Give up and revert back to generating the gruesome
   unsafePerformIO blob below.
   GHC_casm  -> 
     return [Constant ty 
	      (map (\ (hname, cname) -> 
		      (mkHaskellVar (haskelliseName mangle pre hname), cname)) defs)]
-}
   _	     -> do
     lss <- mapM fillIn procs
     return (concat lss)
 where
  procs =   
    [ ProcSpec (noSrcLoc, hname, Nothing, Nothing, ty)
               Nothing
	       (Just (False,"res1="++cname))
	       Nothing
	       Nothing
	       Nothing | (hname, cname) <- defs ]

fillIn (Enum nm ty derivings defs) = do
  pref   <- getPrefixes
  mangle <- getNameMangleFlag
  let defs' = map (\ (hname, cname) -> (haskelliseName mangle pref hname, cname)) defs
      d'    = Enum nm ty derivings defs'
  tgt <- getTarget
  case tgt of
   GHC_casm  -> return [d']
   _	     -> do
     lss <- mapM fillIn procs
     return (d':concat lss)
   where
    procs =   
     [ ProcSpec (noSrcLoc, hname, Nothing, Nothing, ty)
                Nothing
	        (Just (False,"res1="++cname))
	        Nothing
	        Nothing
	        Nothing | (hname, cname) <- defs ]

fillIn d = return [d]
\end{code}

%************************************************************************
%*									*
\subsection{Generating DISs from Types}
%*									*
%************************************************************************

@typeToDIS@ turns a type into a DIS.  It is used if a procedure specification
doesn't contain a @%call@ or a @%result@ annotation.

\begin{code}
typeToDIS :: DISEnv -> Type -> (Call, DIS)
typeToDIS env ty = (call, res)
 where
  ty_args = typeArgs   ty
  ty_res  = typeResult ty

  call = runNS (mapM go ty_args)  "arg"
  res  = runNS (go ty_res) "res"

  go :: Type -> NSM DIS

  go (TypeTuple ts) = do
    ds <- mapM go ts
    return (apply Tuple ds)

  go (TypeList (TypeVar nm _)) =  do
    ptr <- getNewName
    len <- getNewName
    return (apply (Var ("listLen" ++ nm)) [Var ptr, Var len])

  go (TypeVar typeName _) = do
    ns <- getNewNames arity
    return (apply (Var disName) (map Var ns))
   where
     arity :: Int
     arity = maybe 1 (length . fst) x

     disName :: Name 
     disName = lowerName typeName

     x :: Maybe ([Name], DIS)
     x = lookup disName env

   -- From a type application of the form,
   --
   -- (T1 t1 ... tn)
   -- 
   -- you'll get the DIS application
   -- 
   -- (t1 arg1)
   -- 
   -- if t1..tn are all tyvars.
   --
  go (TypeApply (TypeVar f _) args) 
    | allTyVars args = do
       n <- getNewName 
       return (apply (Var disName) [Var n])
    | otherwise = do
       ds <- mapM go args
	 -- how could 'ns' be anything other than the empty list?
       ns <- getNewNames (max 0 (arity - length ds))
       return (apply (Var disName) (ds ++ map Var ns))
   where
     allTyVars argz = all (isTyVar) argz

     isTyVar (TypeVar (n:_) _) = isLower n
     isTyVar _		       = False

     arity :: Int
     arity = maybe 1 (length . fst) x

     disName :: Name 
     disName = lowerName f

     x :: Maybe ([Name], DIS)
     x = lookup disName env

  go t = error ("typeToDIS: unexpected type " ++ show t)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Filling in CCode lines}
%*                                                                      *
%************************************************************************

NB: The DISs should have been expanded before we call this puppy.

\begin{code}

expandCCode :: String -> Call -> DIS -> CCode
expandCCode name ds rs = 
  (False, lhs ++ name ++ "(" ++ concat (intersperse ", " args) ++ ")")
  where
    args = concatMap leafVarsOfDIS ds
    res  = leafVarsOfDIS rs
    lhs | null res  = "" 
	| otherwise = head res ++ " = "

-- like freeVarsOfDIS but omits "functions"

leafVarsOfDIS :: DIS -> [Name]
leafVarsOfDIS = free
 where
  free (Apply _ ds)  = concatMap free ds
  free (Var nm)      = [nm]
  free (Declare _ d) = free d 
  free _             = []

\end{code}

\begin{code}
-- 
-- should you want to transform the function name
-- given on the %fun line in any way, the function
-- below is the one to tweak.
-- 
mangleFunName :: Maybe Name -> String -> Type -> String
mangleFunName (Just nm)  _ _ = nm
mangleFunName Nothing   nm _ = nm
\end{code}
