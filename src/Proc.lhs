%
% Copyright (C) 1997, 2002-2003 Thomas Nordin and Alastair Reid
%

\begin{code}

module Proc ( genProcs ) where

import Text.PrettyPrint
import PrettyUtils -- pretty much everything.

import Name
import Type ( isNonIOType, ppType, Type(..) )
import DIS  ( DIS(..), simplify )
import Decl
import Casm
import Target ( Target )
import MarshallMonad
import ErrMonad
import ListUtils ( insertIfMissing, lowerName, upperName,
		   escapeChar
		 )
import Data.Maybe ( fromMaybe, isJust, fromJust )
import Data.List  ( unzip4, unzip5 )

\end{code}

\begin{code}
genProcs :: Target
         -> Bool
	 -> Bool
	 -> Bool
	 -> (String, String)
	 -> String
	 -> [Decl]
	 -> ([String], [(Doc, Doc, Doc, Doc)])
genProcs target safeCalls debugStub forH14 loc mod_name decls =
  runPMs (map (genProc target safeCalls debugStub forH14 mod_name) decls) loc
\end{code}

%************************************************************************
%*									*
\subsection{Generating function declarations}
%*									*
%************************************************************************

@genProc target gcSafe proc@ returns a triple @(hcode, ccode, entry, header)@
where

o @hcode@ is a Haskell function (and type declaration) which marshalls
  its arguments, performs a casm and unmarshalls the results.

o @ccode@ is any C code that has to be generated as part of the casm.
  (It is empty if the target is GHC.)

o @entry@
  - optional C prototypes to stub functions that performs the
    actual call (and unmarshals the result(s))

o @header@
  - optional Haskell declaration for Haskell stub.

\begin{code}
genProc :: Target
	-> Bool
	-> Bool
	-> Bool
	-> String
	-> Decl
	-> PM (Doc, Doc, Doc, Doc)
genProc target gcSafe debugStubs forH14 mod_name
		    (ProcSpec (loc, name, _, mb_ctxt, typ)
		              (Just ds)
		              (Just (isSafe, ccode))
		              ofs
		              (Just res)
		              mbend) = do
  (av, unpack, ddecls, adecls, as)  <- liftErr (marshallDISs loc target ds)
  (pack, rv, rdecls, rs)	    <- liftErr (unmarshallDIS loc target res)
  (cconv, ext_name)		    <- getSpecLoc
  let
    results               = rs ++ fs
    end
      | not (isJust mbend) = text ""
      | otherwise	   = text (insertIfMissing ';' (fromJust mbend))

    entries GHC_ccall = entry
    entries FFI       = entry
    entries _         = entry <> comma

    casm_start =
      case target of
        GHC_ccall -> rdecls <> fdecls <> ddecls
        FFI -> rdecls <> fdecls <> ddecls
	_   -> adecls <> ddecls <> rdecls <> fdecls

    casm = Casm name
		ext_name
		cconv
                ffi_hfile
		(gcSafe || isSafe)
                casm_start
	        (ppText (escapePercent (insertIfMissing ';' ccode)) $$ if_no_failc)
                end
                as results

    -- nasty hack - should have it passed in from elsewhere
    ffi_hfile = mod_name ++ "_stub_ffi.h"

    -- GHC _casm_ strings treat '%' specially, so we need to
    -- escape any occurrences of it within the user code chunk.
    escapePercent cs
      | target == GHC_casm = escapeChar '%' '%' cs
      | otherwise          = cs

    (call, hdecl, cprotos, cdecl, entry) = ppCasm target mod_name debugStubs casm

    {-
     The unmarshalling code avoids generating code of the
     form "call >>= \ (res1, ... resm) -> (res1, ... resm)"
     by performing a very simple-minded test.

     Since the GHC backend tries to avoid unnecessary uses
     of (return) and (>>=), it binds the result values when
     printing out the Haskell code interfacing with the
     C code inside %call, but doesn't tuple them up and
     return at the end of the Haskell code block returned
     from ppCasm. The upshot of this change is that we
     cannot take the below optimisation, as we always
     have to finish off the Haskell stub with a `return'.
     Not optimal, but it will have to do for now.

     ToDo: use an abstract Haskell data type and delay
     the conversion into Doc until we're ready to output
     the final code.

		    -- 2/98 SOF
    -}
    hs_result
      = call $$ (if_no_failh (pack res1))
          where
	    res1 =
	      case results of
	        [] -> empty
		_  -> ppReturn rv

  return (
            text name <+> text "::" <>
	    (if isJust mb_ctxt then
	        empty <+> ppType (fromJust mb_ctxt) <+> text "=>"
	     else
	        empty) <+> ppType typ
         $$ text name <+>  hsep av   <+> text "="
         $$ indent (
                wrap
      	      $ unpack
      	      $ hs_result
      	      )
         $$ hdecl
         , cdecl
         , entries target
         , cprotos
         )
   where
    wrap d = if isNonIOType typ then is_unsafe d else d

    is_unsafe :: Doc -> Doc
    is_unsafe d = text "unsafePerformIO(" $$ indent d <> text ")"

    ofs'   = fromMaybe [] ofs

    (fdecls, fs, if_no_failc, if_no_failh) = failureHandling forH14 ofs'

genProc _ _ _ _ _ (Haskell code)       = return (text code, empty, empty, empty)
genProc _ _ _ _ _ (C code)             = return (empty, text code, empty, empty)
genProc _ _ _ _ _ (DllName nm)	   = do
   (cconv, ext_loc) <- getSpecLoc
   setSpecLoc (cconv, nm)
   return (empty,empty,empty,empty)
genProc _ _ _ _ _ (CConv nm)	   = do
   (_, ext_loc) <- getSpecLoc
   setSpecLoc (nm, ext_loc)
   return (empty,empty,empty,empty)
genProc tgt _ _ _ _ (Constant ty defs)
 | tgt /= GHC_casm = error "panic!: genProc.Const - not supposed to happen."
 | otherwise       =
   let
     cons          = map fst defs
     lit_lit x     = text ("``"++x++"''")

     mkDef (hname, cname) =
       text hname <+> equals <+> (lit_lit cname)

     haskell =
       hsep (punctuate comma (map text cons)) <+> text "::" <+> ppType ty $$
       vcat (map mkDef defs)
   in
   return (haskell, empty, empty, empty)

genProc target _ _ _ _ (Enum nm ty derivs defs) = do
  let
   cons          = map (fst) defs
   rhs (x, y)
    | target == GHC_casm = text ("``"++y++"''")
    | otherwise		 = text (lowerName x)

   marshall_nm   = text ("marshall_" ++ nm)
   unmarshall_nm = text ("unmarshall_" ++ nm)
   mkIf (h,c) hole =  ppIf (text "arg1 ==" <+> rhs (h,c))
			   (text (upperName h))
			   hole
   marshall_alts   = map ( \ (con, val) -> (text (upperName con), rhs (con,val))) defs
   unmarshall_alts = foldr mkIf (text ("error (\"unmarshall_"++nm++":\
                                       \ unknown value (\"++show arg1++\")\\n\")"))
			        defs

   marshall_fun  =
     marshall_nm <+> text "::" <+> ppType (Arrow (TypeVar nm Nothing) ty) $$
     hang (marshall_nm <+> text "arg1 = ")
      2 (ppCases (text "arg1") marshall_alts)

   unmarshall_fun  =
     unmarshall_nm <+> text "::" <+> ppType (Arrow ty (TypeVar nm Nothing)) $$
     hang (unmarshall_nm <+> text "arg1 = ")
      2   unmarshall_alts

   haskell =
     hang (text "data" <+> text nm)
      2   (vcat (zipWith (<+>) (equals:repeat (text "|"))
                               (map (text.upperName) cons) ) $$
           (text "deriving" <+> ppTuple (map text derivs))) $$
     marshall_fun $$
     unmarshall_fun

  return (haskell, empty, empty, empty)
genProc _ _ _ _ _ _            = return (empty, empty, empty, empty)

\end{code}

%************************************************************************
%*									*
\subsection{Error Handling}
%*									*
%************************************************************************

\begin{code}

failureHandling :: Bool -> Fail -> (Doc, [Param], Doc, Doc -> Doc)
failureHandling _      [] = (empty, [], empty, id)
failureHandling forH14 rs =
  ( ppCDecl "int" fn $$ ppCDecl "char*" fs
  , [ Param fn fn fn "int" (Int Natural) False
    , Param fs fs fs "void*" Ptr False
    ]
  , ppCIf [ ( parens (textline [ fn, "= (", p, ")" ])
            , textline [ fs, "=", s, ";" ]
            )
          | (p, s) <- rs ]
          (Just (textline [ fn, "= 0;" ]))
  , \d -> ppIf (textline ["(", fn, "/= (0::Int))"])
               (textline ["unmarshall_string_", fs, ">>= " , fail_nm , " . userError"])
               d
  )
  where
    fn = "gc_failed"
    fs = "gc_failstring"

    fail_nm
      | forH14    = "fail"
      | otherwise = "ioError"

\end{code}


%************************************************************************
%*									*
\subsection{Generating the packing and unpacking of DISs}
%*									*
%************************************************************************

\begin{code}

marshallDISs  :: SrcLoc -> Target -> [DIS] -> ErrM String ([Doc], Doc -> Doc, Doc, Doc, [Param])
marshallDISs loc t ds = do
 res <- runMarshallM (mapM ((\d -> setDISContext d (marshall d)).simplify) ds)
                     loc t "gc_arg"
 let (ns, ms, ddecls, decls, pss) = unzip5 res
 return (map text ns, foldr (.) id ms, hsep ddecls, hsep decls, concat pss)

unmarshallDIS :: SrcLoc -> Target -> DIS -> ErrM String (Doc -> Doc, Doc, Doc, [Param])
unmarshallDIS loc t d =
 runMarshallM (((\ d1 -> setDISContext d1 (unmarshall d1)).simplify) d)
              loc t "gc_res"

\end{code}


%************************************************************************
%*									*
\subsection{Marshalling code}
%*									*
%************************************************************************

@marshall dis@ returns a 4-tuple @(pat, unpack, decls, leaves)@ where

o @pat@ is a pattern to match against
o @unpack@ is the code to do the unpacking
o @decls@ is a bunch of C declarations for the variables it unpacks into
o @leaves@ is the list of values it generates

eg @marshall loc (%%Int {int} x, %%Float {float} {0.0}, foo (int z))@ returns
something like this:
@
  ( (x,y,gc_arg1)
  , \ rest -> case marshall_foo gc_arg1 of { z -> rest }
  , text "int x; float arg2; int z;"
  , [ Param "x" "x" "int" (Int Natural) False,
      Param "arg2" "0.0" "float" Float False,
      Param "z" "z" "int" (Int Natural) False]
  )
@

\begin{code}

marshall :: DIS -> MarshallM (Name, Doc -> Doc, Doc, Doc, [Param])

marshall (Var v) = -- no kind info so it's not a casm argument (very weird)
 return ( v, id, empty, empty, [])

marshall (Apply (Declare cty (Var v)) [d]) = do
  (n, m, ddecl, decl, ps) <- marshall d
  new_v <- getNewName
  let
     -- for the variable that has been given a new C type,
     -- flag its parameter as such, so that the variable
     -- being bound to the resulting casted value will be
     -- properly initialised.
    castTo _    []     = []
    castTo cty1 (y:ys)
     | v == haskellName y = y{cName=new_v,
     			      cType=cty1,
			      cExpr=v,
			      needsDecl=True} : ys  -- there's only one.
     | otherwise	  = y:castTo cty1 ys

  return (n, m, ppCDecl cty v, empty{-or should that be decl?-}, castTo cty ps)

marshall (Apply (Declare _ (CCode _)) [d]) = marshall d

marshall (Apply (Kind k) [ Apply (CCode cty) [Var v] ]) =
  return ( v, id, empty, ppCDecl cty v, [Param v v v cty k False])

marshall (Apply (Kind k) [ Apply (CCode cty) [CCode e] ]) = do
  v <- getNewName
  return ( v, id, empty, empty, [Param v v e cty k True])

marshall Tuple = do             -- trivial case, probably not used
  v <- getNewName
  let unpack = ppCase (text v) (ppTuple [])
  return (v, unpack, empty, empty, [])

marshall (Apply Tuple ds) = do
  v <- getNewName
  bits <- mapM marshall ds
  let (ns, ms, ddecls, decls, pss) = unzip5 bits
  let unpack = ppCase (text v) (ppTuple (map text ns))
  return (v, compose (unpack:ms), hsep ddecls, hsep decls, concat pss)

marshall (Apply (Constructor c) ds) = do
  v <- getNewName
  bits <- mapM marshall ds
  let (ns, ms, ddecls, decls, pss) = unzip5 bits
  let unpack = ppCase (text v) (ppApply (text c) (map text ns))
  return (v, compose (unpack:ms), hsep ddecls, hsep decls, concat pss)

marshall (Apply (Record c fs) ds) = do
  v <- getNewName
  bits <- mapM marshall ds
  let (ns, ms, ddecls, decls, pss) = unzip5 bits
  let unpack = ppCase (text v)
                      (ppRecord (text c)
                                (map text fs)
		        	(map text ns))
  return (v, compose (unpack:ms), hsep ddecls, hsep decls, concat pss)

marshall (Apply (UserDIS io k ma _) [d]) = do
  v <- getNewName
  (n, m, ddecl, decl, ps) <- marshall d
  let unpack
       | io        = \ doc -> ppBind
                              (ppApply (text ma) [text v])
			      (text n, doc)
       | otherwise = ppCase (ppApply (text ma) [text v]) (text n)
  case k of
    Nothing -> return (v, (unpack.m), ddecl, decl, ps)
    Just pk -> do
     t <- getTarget
     return (v, (unpack.m), ddecl, decl, [Param n n n (kindToCType t pk) pk False])

marshall (Apply (UserDIS io k ma _) ds) = do
  v <- getNewName
  bits <- mapM marshall ds
  let (ns, ms, ddecls, decls, pss) = unzip5 bits
  let unpack
       | io        =
         \ d -> ppBind
                  (ppApply (text ma) [text v])
		  (ppTuple (map text ns), d)
       | otherwise = ppCase (ppApply (text ma) [text v]) (ppTuple (map text ns))
  case k of
    Nothing -> return (v, compose (unpack:ms), hsep ddecls, hsep decls, concat pss)
    Just _  -> do
     t <- getTarget
     return (v, compose (unpack:ms), hsep ddecls, hsep decls, (concat pss))

marshall a@(Apply (Var ('%':t)) ds)
  | t == "Maybe"   =
    case ds of
      [ CCode nothing, just ] -> marshallMaybe nothing just
      _ -> bombWith ("marshall: Malformed %Maybe " ++ show a)
  | t == "ForeignPtr" =
    case ds of -- This is just a special case of giving a kind
    [CCode cty, arg, CCode free]
      -> do
           v <- getNewName
           (a, unpacka, ddecls, decls, ps) <- marshall arg
           let -- \ doc -> withForeignPtr ( \ v -> doc )
               unpack doc =  text "withForeignPtr" <+> text a
                             <+> lparen <> text "\\" <+> text v <+> text "->"
                          $$ doc <> rparen
           return ( a
                  , unpack
                  , ddecls
                  , decls
                  , Param v a a cty (ForeignPtr free) False
                    : ps
                  )
    _ -> bombWith ("marshall: Malformed %ForeignPtr " ++ show a)
  | t == "Foreign" =
    case ds of -- This is just a special case of giving a kind
    [CCode cty, arg, CCode free]
      -> let
           cres = case arg of { Var v -> v; CCode res -> res; _ -> error "marshall:cres" }
         in
         return ( cres
                , id
		, empty
		, ppCDecl cty cres
		, [Param cres cres cres cty (Foreign free) False]
		)
    _ -> bombWith ("marshall: Malformed %Foreign " ++ show a)
  | otherwise
  = bombWith ("marshall: Unrecognised DIS %" ++ t)

marshall (Apply (Var _) [Var _]) = do
  orig <- getDISContext
  bombWith ("marshall: Don't know how to marshall " ++ show orig)

marshall (Apply (Var t) ds) = do
  nm   <- getNewName
  bits <- mapM marshall ds
  let (ns, ms, ddecls, decls, pss) = unzip5 bits
  return ( nm
         , \e -> ppApply fun [text nm] `ppBind`
	         (ppTuple (map text ns), compose ms e)
         , hsep ddecls
         , hsep decls
	 , concat pss
	 )
 where
  fun = text ("marshall_" ++ t)

marshall _ = do
  orig <- getDISContext
  bombWith ("marshall: Don't know how to marshall " ++ show orig)

\end{code}

The marshalling of @%Maybe nothing just@ is a little different.
We generate code like this:
@
      case x of
      Nothing -> return nothing -- should be a tuple
      Just j  -> <unpack j> >>= \ ... ->
                 return (j1,..jn)
                                             >>= \ (j1 ... jn) -> <hole>
@
where @j1@ ... @jn@ are the leaf vars of the Just dis.

\begin{code}

marshallMaybe :: String -> DIS -> MarshallM (Name, Doc -> Doc, Doc, Doc, [Param])
marshallMaybe nothing just = do
      (j, unpackj, ddecls, decls, ps) <- marshall just
      let js = ppTuple (map (text.haskellName) ps)
	  unpack = ppCases (text j)
		 [ (text "Nothing",            ppReturn (text nothing))
		 , (ppApply (text "Just") [text j], unpackj (ppReturn js))
		 ]
      return ( j
             , \ hole -> parens unpack `ppBind` (js, hole)
	     , ddecls
	     , decls
	     , ps
	     )

\end{code}

%************************************************************************
%*									*
\subsection{UnMarshalling code}
%*									*
%************************************************************************

@unmarshall dis@ returns a quadruple @(pack, returnValue, decls, leaves)@ where

o @decls@ is a bunch of C declarations for return values

o @leaves@ is a list of parameters that should be returned from the casm.

o @pack@ is an expression builder which constructs parts of the result
  (by calling user marshalling code).   It is of the form:

  \ hole ->
       text "unmarshall_T1 x1 >>= \ y1 ->"
    $$ text "unmarshall_T2 x2 >>= \ y2 ->"
    $$ hole

o @returnValue@ is an expression which builds the return values from the
  @leaves@.

\begin{code}

unmarshall :: DIS -> MarshallM (Doc -> Doc, Doc, Doc, [Param])

unmarshall (Var v) -- no kind info so it's not a casm argument
  = return ( id, text v, empty, [])

unmarshall (CCode c)
  = return ( id, text c, empty, [])

unmarshall (Apply (Declare cty (Var v)) [d]) = do
  (pack, m, decl, ps) <- unmarshall d
  return (pack, m, ppCDecl cty v, castTo cty ps)
 where
  castTo _    [] = []
  castTo cty1 (p@(Param x c_nm _ _ k n):ps)
    | v == x    = (Param x c_nm (cast cty1 x) cty1 k n):ps
    | otherwise = p:castTo cty1 ps

unmarshall (Apply (Declare _ (CCode _)) [d]) = unmarshall d

unmarshall (Apply (Kind k) [ Apply (CCode cty) [Var v] ])
  = return ( id, text v, ppCDecl cty v, [Param v v (cast cty v) cty k False])

unmarshall (Apply (Kind k) [ Apply (CCode cty) [CCode e] ]) = do
  v <- getNewName
  return ( id, text v, empty, [Param v v (cast cty e) cty k False])

unmarshall Tuple = do
  return ( id, ppTuple [], empty, concat [])

unmarshall (Apply Tuple ds) = do
  bits <- mapM unmarshall ds
  let (packs, ms, decls, pss) = unzip4 bits
  return ( compose packs, ppTuple ms, hsep decls, concat pss)

unmarshall (Apply (Constructor c) ds) = do
  bits <- mapM unmarshall ds
  let (packs, ms, decls, pss) = unzip4 bits
  return (compose packs, ppApply (text c) ms, hsep decls, concat pss)

unmarshall (Apply (Record c fs) ds) = do
  bits <- mapM unmarshall ds
  let (packs, ms, decls, pss) = unzip4 bits
  return (compose packs, ppRecord (text c) (map text fs) ms, hsep decls, concat pss)

unmarshall (Apply (UserDIS io k _ unma) ds) = do
  v    <- getNewName
  bits <- mapM unmarshall ds
  let (packs, ms, decls, pss) = unzip4 bits

      call_unmarshall = ppApply (text unma) [ppTuple ms]
      pack hole
        | io &&
          isEmpty hole = call_unmarshall
        | io           = ppBind call_unmarshall (text v, hole)
        | otherwise    = ppLet v call_unmarshall $$ hole


      {- HACK!!!:
         if no unmarshalling of the arguments to the
	 UserDIS is required (list of Vars), we re-use
	 the return values (we *know* that the Docs
	 represent variable names.) Kludge a bogus
	 Kind onto the Params while we're at it.
      -}
      Just kind   = k
      results     =
        case concat pss of
	  [] -> map (\ x -> Param (show x) (show x) (show x) "" kind False) ms
          ls -> ls

  return (compose packs . pack, text v, hsep decls, results)

unmarshall v@(Apply (Var ('%':t)) ds)
  | t == "Maybe"
  = case ds of
     [ CCode nothing, just ] -> unmarshallMaybe (text nothing) just
     _ -> bombWith ("unmarshall: Malformed %Maybe " ++ show v)
  | t == "ForeignPtr"
  = case ds of -- This is just a special case of giving a kind
      [CCode cty, a, CCode free] -> do
         v0 <- getNewName
         v1 <- getNewName
         v2 <- getNewName
         let
           cres = case a of { Var v3 -> v3; CCode res -> res; _ -> error "unmarshall:cres" }

	    {-
	      Only declare results when they're of the form
	        "%ForeignPtr {cty} x {finaliser}"

	      falling into line with what is done for non-magical
	      prim types.
	    -}
	   decl_cres =
	      case a of
	         Var _    -> ppCDecl cty cres
		 CCode _  -> empty
		 _        -> error "unmarshall:decl_cres"

            -- ForeignPtr constructor:
	    --   newForeignPtr :: Ptr a -> FinalizerPtr a -> IO (ForeignPtr a)
            --
           newForeignPtr = text "newForeignPtr"

            -- Check to see if we need to prepend an ampersand..
	    -- (the ampersand is strictly speaking not necessary, but
	    -- it leads to arguably clearer code.)
           free' = case free of '&':_ -> free ; _ -> '&':free

           pack c = ppApply newForeignPtr [text v2, text v0] `ppBind` (text v1, c)
         return ( pack
                , text v1
		, decl_cres
	        , [ Param v2 v2 (cast "HsFunPtr" free') "HsFunPtr" FunPtr False
	          , Param v0 v0 (cast cty cres)         cty        Ptr    False
	          ]
	        )

      _ -> bombWith ("unmarshall: Malformed %ForeignPtr " ++ show v)
  | t == "Foreign"
  = case ds of -- This is just a special case of giving a kind
      [CCode cty, a, CCode free] -> do
         v0 <- getNewName
         v1 <- getNewName
         v2 <- getNewName
         let
           cres = case a of { Var v3 -> v3; CCode res -> res; _ -> error "unmarshall:cres" }

	    {-
	      Only declare results when they're of the form
	        "%Foreign {cty} x {finaliser}"

	      falling into line with what is done for non-magical
	      prim types.
	    -}
	   decl_cres =
	      case a of
	         Var _    -> ppCDecl cty cres
		 CCode _  -> empty
		 _        -> error "unmarshall:decl_cres"

            -- ForeignObj constructor:
	    --   makeForeignObj :: Addr -> Addr{-finaliser-} -> IO ForeignObj
            --
           mkForeignObj = text "makeForeignObj"

            -- Check to see if we need to prepend an ampersand..
	    -- (the ampersand is strictly speaking not necessary, but
	    -- it leads to arguably clearer code.)
           free' = case free of '&':_ -> free ; _ -> '&':free

           pack c = ppApply mkForeignObj [text v0, text v2] `ppBind` (text v1, c)
         return ( pack
                , text v1
		, decl_cres
	        , [ Param v2 v2 free' "void*" Addr False
	          , Param v0 v0 (cast cty cres) cty Addr False
	          ]
	        )

      _ -> bombWith ("unmarshall: Malformed %Foreign " ++ show v)
  | otherwise
  = bombWith ("unmarshall: Unrecognised DIS %" ++ t)

unmarshall (Apply (Var _) [Var _]) = do
  orig <- getDISContext
  bombWith ("Don't know how to unmarshall " ++ show orig)

unmarshall (Apply (Var t) ds) = do
  nm   <- mapMarshallM text getNewName
  bits <- mapM unmarshall ds
  let (packs, ms, decls, pss) = unzip4 bits
      pack c = ppApply fun ms `ppBind` (nm, c)
  return (compose packs . pack, nm, hsep decls, concat pss)
 where
  fun = text ("unmarshall_" ++ t)

unmarshall _ = do
  orig <- getDISContext
  bombWith ("Don't know how to unmarshall " ++ show orig)

\end{code}

Again, the unmarshalling of @%Maybe nothing just@ is a little different.
We generate code like this:
@
  if (nothing == (j1, ...jn))
  then return Nothing
  else <pack j1 .. jn>
       return Just ( ... )              >>= \ v -> <hole>
@
where @j1@ ... @jn@ are the leaf vars of the Just dis.

\begin{code}

unmarshallMaybe :: Doc -> DIS -> MarshallM (Doc -> Doc, Doc, Doc, [Param])
unmarshallMaybe nothing just = do
  nm   <- mapMarshallM text getNewName
  (pack, m, decls, ps) <- unmarshall just
  let js = map (text.haskellName) ps
  return ( \ hole ->
             parens (
               ppIf (nothing <+> text "==" <+> ppTuple js)
                  (text "return Nothing")
                  (pack $ ppReturn (ppApply (text "Just") [m])))
             `ppBind` (nm, hole)
         , nm
	 , decls
         , ps
         )

\end{code}


%************************************************************************
%*									*
\subsection{Utilities}
%*									*
%************************************************************************

\begin{code}

compose :: [ (a -> a) ] -> (a -> a)
compose = foldr (.) id

cast :: String{-c type-} -> String{-expr-} -> String
cast cty e = ("("++cty++')':'(':e++")")

-- bombWith adds another error message to
-- the list of error messages and causes
-- the processing of a procedure specification
-- to be aborted.
bombWith :: String -> MarshallM a
bombWith err = do
  loc <- getSrcLoc
  failWith (show loc ++ ":\n      " ++ err ++ "\n")

\end{code}

Layer our own little monad on top of the error one:

\begin{code}
data PM a = PM (PMState -> (PMState, ErrM String a))

type PMState = (String,String) -- current callconv and ext dll. name

instance Monad PM where
  return v  = PM (\ x -> (x, return v))
  (>>=) (PM m) f =
    PM (\ x ->
	  case m x of
	    (x', em) ->
		case em of
		  Failed e    -> (x', failure e)
		  Succeeded v ->
			let (PM n) = f v in
			n x')

getSpecLoc :: PM (String,String)
getSpecLoc = PM (\ x -> (x, return x))

setSpecLoc :: (String,String) -> PM ()
setSpecLoc x = PM (\ _ -> (x,return ()))

liftErr :: ErrM String a -> PM a
liftErr err = PM (\ x -> (x, err))

runPMs :: [PM a] -> (String,String) -> ([String], [a])
runPMs ls loc = go loc ls
  where
    go _   [] = ([],[])
    go acc ((PM x):xs) =
       case x acc of
         (acc', res) ->
	    let (as,bs) = go acc' xs in
	    case runErrM res of
	      Left     e -> (e:as,bs)
	      Right    v -> (as,v:bs)

\end{code}


%************************************************************************
%*									*
\subsection{Examples}
%*									*
%************************************************************************

leave out DEBUGGING code.  -- SOF 10/97

begin{code}

-- type Proc = (Sig, Call, Doc, Fail, Result)

proc1 :: Proc
proc1 =
    ( ("foo", TypeVar "Int" `Arrow` TypeVar "Float")
    , [tuple [int "arg1", int "arg2"]]
    , text "res=(float)(arg1+arg2)"
    , [("res>42","\"loser\"")]
    , tuple [flt "res1", int "res2"]
    , text "/* end */"
    )
 where
  int nm   = Apply (Kind (Int Natural)) [ CCode "int",   Var nm ]
  flt nm   = Apply (Kind Float) [ CCode "float", Var nm ]
  tuple ds = (False, Apply Tuple ds)

tst :: Proc -> IO ()
tst p = case genProc FFI False p of { (d1,d2,d3) ->
        putStr $ render (d1 $$ d2 $$ d3)
        }

end{code}
