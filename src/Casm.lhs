%
% Copyright (C) 1997-2003 Thomas Nordin and Alastair Reid
%


\begin{code}
module Casm
	( Casm(..)
	, Kind(..)
	, Size(..)
	, ppKind
	, lookupKind    -- :: String -> Maybe Kind
        , kindToCType   -- :: Kind   -> String
	, Param(..)
	, ppCasm
	, Target(..)
	) where

import Text.PrettyPrint
import PrettyUtils  -- extensions

import Target
import Data.Char   ( isAlphaNum )
\end{code}

%************************************************************************
%*									*
\subsection{Data structures}
%*									*
%************************************************************************

\begin{code}

data Casm 
  = Casm
      String   -- a unique label - in case we need to generate decls for this casm.
      String   -- name of external DLL/.so where the function lives. (FFI b.end only)
      String   -- Calling convention (FFI b.end only.)
      String   -- Include file
      Bool     -- Are we going to be GC safe?
      Doc      -- initialisation code
      Doc      -- C code
      Doc      -- cleanup code
      [Param]  -- arguments
      [Param]  -- results

\end{code}

The @Kind@ type represent the repertoire of primitive Haskell types 
the FFI of a target supports.

\begin{code}

data Kind
  = Int    Size
  | Word   Size
  | Addr   -- deprecated
  | Float
  | Double
  | Char
  | Ptr
  | FunPtr
  | StablePtr
  | ForeignPtr String -- freeing function
  | Foreign String -- freeing function
    -- GHC extensions
  | ByteArr
  | MutByteArr

instance Show Kind where
  show = showKind True

showKind :: Bool -> Kind -> String
showKind withSize k = 
  case k of
    Int sz        -> "Int"  ++ if withSize then ' ':showSize sz else ""
    Word sz       -> "Word" ++ if withSize then ' ':showSize sz else ""
    Addr          -> "Addr"
    Float         -> "Float"
    Double        -> "Double"
    Char          -> "Char"
    Ptr           -> "Ptr"
    FunPtr        -> "FunPtr"
    StablePtr     -> "StablePtr"
    ForeignPtr _  -> "ForeignPtr"
    Foreign _     -> "Foreign"
      -- GHC extensions
    ByteArr       -> "ByteArr"
    MutByteArr    -> "MutByteArr"

data Size 
 = Natural
 | Size Int  -- 8/16/32/64.
   deriving ( Show )

lookupKind :: String -> Maybe Kind
lookupKind s = lookup s' kindNames
 where
  s' = takeWhile isAlphaNum (dropWhile (not.isAlphaNum) s)

kindNames :: [(String, Kind)]
kindNames =
  [ ("Char",   		  Char)
  , ("Int",    		  Int Natural)
  , ("Int8",   		  Int (Size 8))
  , ("Int16",  		  Int (Size 16))
  , ("Int32",  		  Int (Size 32))
  , ("Int64",  		  Int (Size 64))
  , ("Word",   		  Word Natural)
  , ("Word8",  		  Word (Size 8))
  , ("Word16", 		  Word (Size 16))
  , ("Word32", 		  Word (Size 32))
  , ("Word64", 		  Word (Size 64))
  , ("Addr",   		  Addr)
  , ("Float",  		  Float)
  , ("Double",            Double)
  , ("Ptr", 	          Ptr)
  , ("FunPtr", 	          FunPtr)
  , ("StablePtr", 	  StablePtr)
  , ("ByteArray", 	  ByteArr)
  , ("MutableByteArray",  MutByteArr)
  , ("ForeignPtr",        ForeignPtr "")
  , ("ForeignObj",        Foreign "")
  ]

ppKind :: Kind -> Doc
ppKind k = text ("%%" ++ show k)

kindToCType :: Target -> Kind -> String
kindToCType GHC_ccall k  = kindToStgType k
kindToCType FFI       k  = kindToFFIType k
kindToCType _         k  =
 case k of
   -- kindasorta
  Int Natural     -> "int"
  Int (Size 8)    -> "signed char"
  Int (Size 16)   -> "signed short"
  Int (Size 32)   -> "signed long"
  Int (Size 64)   -> "signed longlong"
  Int _           -> error "kindToCType: unknown int size"
  Word Natural   -> "unsigned int"
  Word (Size 8)  -> "unsigned char" 
  Word (Size 16) -> "unsigned short" 
  Word (Size 32) -> "unsigned long" 
  Word (Size 64) -> "unsigned longlong" 
  Word _         -> error "kindToCType: unknown word size"
  Addr          -> "void *"
  Float         -> "float"
  Double        -> "double"
  Char          -> "char"
  Ptr           -> "void*"
  FunPtr        -> "void (*)(void)"
  StablePtr     -> "long"
  (ForeignPtr _)-> "void *"
  (Foreign _)   -> "void *"
  ByteArr      -> "void *"
  MutByteArr   -> "void *"

kindToStgType :: Kind -> String
kindToStgType k =
 case k of
   ForeignPtr _  -> "StgAddr" -- foreign objs have been turned into addrs by
   			      -- the time they reach C code.
   Foreign _  -> "StgAddr" -- foreign objs have been turned into addrs by
   			   -- the time they reach C code.
   _ -> 
    "Stg" ++
     case k of 
   	Int  s     -> "Int" ++ showSize s
   	Word s     -> "Word" ++ showSize s
   	Addr       -> "Addr"
   	Float      -> "Float"
   	Double     -> "Double"
   	Char       -> "Char"
   	Ptr        -> "Ptr"
   	FunPtr     -> "Ptr"
   	StablePtr  -> "StablePtr"
   	ByteArr    -> "ByteArray"
   	MutByteArr -> "ByteArray"
	Foreign{}  -> "ForeignObj"
	ForeignPtr{}-> "ForeignPtr"


showSize :: Size -> String
showSize s = 
 case s of 
   Natural -> ""
   Size 8  -> "8"
   Size 16 -> "16"
   Size 32 -> "32"
   Size 64 -> "64"
   Size z  -> error ("showSize: unknown size " ++ show z)

kindToFFIType :: Kind -> String
kindToFFIType k =
 case k of
   ForeignPtr _-> "HsPtr" -- foreign objs have been turned into addrs by
   			  -- the time they reach C code.
   Foreign _  -> "HsPtr"  -- foreign objs have been turned into addrs by
   			  -- the time they reach C code.
   Int s      -> "HsInt" ++ showSize s
   Word s     -> "HsWord" ++ showSize s
   Addr       -> "HsPtr"
   Float      -> "HsFloat"
   Double     -> "HsDouble"
   Char       -> "HsChar"
   Ptr        -> "HsPtr"
   FunPtr     -> "HsFunPtr"
   StablePtr  -> "HsStablePtr"
    -- GHC extensions
   ByteArr    -> "StgByteArray"
   MutByteArr -> "StgByteArray"


\end{code}

The @Param@ type encodes a value that is either an argument
or a result. 

\begin{code}
data Param = 
  Param {
	-- the name to use on the Haskell side to bind
	-- the parameter value to.
    haskellName :: String,  -- the name to use name to be used where?
        -- the name that the parameter is bound to inside C chunks.
    cName	:: String,
	-- the expression the parameter is initially bound to.
    cExpr	:: String,
	-- the C type of the parameter.
    cType	:: String,
        -- the basic Haskell type of the parameter.
    paramKind	:: Kind,
	-- for parameters that are cast to another type before being
	-- used inside a C chunk (via a %declare DIS), the cast needs
	-- to be performed at some stage. By setting the 'needsDecl'
	-- flag, code to initialise/cast a 'declare'd variable will
	-- be emitted by the code backend.
	-- 
	-- [ This flag is only ever looked at by the backends that generate
	--   a separate C function stub to hold the body of a procedure
	--   specification (i.e., they cannot express this inline as with
	--   _casm_s and the like.) 'Normal' parameters are then bound
	--   to C function parameter names, but ones that are declared
	--   needs to be handled specially, hence the need for this extra
	--   field in the Param type.
	-- ]
    needsDecl   :: Bool
  }

getKind :: Param -> Kind
getKind p = paramKind p
\end{code}


%************************************************************************
%*									*
\subsection{The main entry points}
%*									*
%************************************************************************

ToDo:

o If the arg names overlap with the result names, we should either:
    - not redeclare the result holder (assuming the same name is used)
     or
    - report an error

    - ignore it

  since it is almost certainly not what the user wanted and they
  can easily rename to avoid the warning.

@ppCasm@ produces C/Haskell code, returning a four tuple containing:
 
  - Haskell code to invoke Haskell/C code that interfaces to
    the external function we're interfacing to. GHC backends also
    bind the Haskell results (if any) to variable names. This
    is done to try to generate code that doesn't use `return'
    needlessly.
  - optional Haskell declaration for Haskell stub.
  - optional C prototypes to stub functions that performs the
    actual call (and unmarshals the result(s))
  - optional C code that massages arguments/results around the
    call to the actual function we're interfacing to.

GHC uses only the first, inlining the function/code block we want
to call upon from Haskell.

\begin{code}
ppCasm :: Target 
       -> String
       -> Bool 
       -> Casm 
       -> ( Doc
	  , Doc
	  , Doc
	  , Doc
	  , Doc
	  )

ppCasm FFI _ debugStubs (Casm name ext_name cconv hfile gcsafe start ccode end args results) =
 ( (text name') <+> hsep (map (text.haskellName) args) $$ pop
 , text "foreign import " <+> text cconv <+> safecall <+> ext_loc <+>
   text name' <+> text "::" <+> prim_type $$
   access_decls
 , proto_stub $$ access_proto_ghc -- haskell decl.
 , ppPrimStgDecl c_type
    	(  declareResult
        $$ start
	$$ stubDebugCode debugStubs name
    	$$ hsep (map initArg args)
	$$ ppBlock (ccode $$ withSemi push)
        ) $$ access_stubs
 , proto $$ access_proto
 )
 where
    ext_loc = doubleQuotes (
      (text hfile) <+>
      (if null ext_name then empty else brackets (text ext_name)) <+>
      text name')

    safecall
     | gcsafe    = empty
     | otherwise = text "unsafe"

    name'      = addPrimPrefix name
    proto      = text "extern" <+> c_type      <> semi
    proto_stub = text "extern" <+> c_type_stub <> semi
    prim_type  = ppType FFI (map getKind args) prim_result_ty
    prim_result_ty 
      | not (null results) && not (null (tail results)) = [Ptr] 
              -- length results > 1
      | otherwise	   = map getKind results
      

    c_type_stub = ret_type_stub   <+> 
                  ppSimpleApply name' ((map (arg_stub) args) `sepdBy` comma)
    c_type = ret_type   <+> 
             ppSimpleApply name' ((map (arg) args) `sepdBy` comma)

    arg_stub p = text (cType p) <+> text (haskellName p)

    arg p = text (cType p) <+> text (cName p)
    
     {-
      The C FFI stub returns void, a prim type or a pointer to
      a structure holding multiple results.
     -}
    (ret_type,ret_type_stub) =
      case results of
	 []  -> let v = text "void" in (v,v)
	 [p] -> (text (cType p), text (cType p)) --kindToFFIType (paramKind p)))
	 _   -> (text "void*", text "void*")

    (declareResult, push, pop, access_stubs, access_proto_ghc, access_proto, access_decls) = 
                                 resultPassing FFI (cconv, hfile, ext_name) name' end results

      -- for the FFI stub generated from an Casm, initialise any parameters that
      -- are passed in at type different to which it is used inside the stub body.
      -- (i.e., for any parameter that subsequently falls under the spell of a 
      --  %declare DIS.)
    initArg p | needsDecl p = ppAssign (cExpr p)
    				       (parens (text (cType p)) <> 
				         text (cName p))
    initArg _		    = empty

ppCasm GHC_ccall _ debugStubs (Casm name _ _ _ gcsafe start ccode end args results) =
 ( (ccall gcsafe) (text name') (map (text.haskellName) args) $$
   pop
 , empty
 , proto_ghc $$ access_proto_ghc -- haskell decl.
 , ppPrimStgDecl c_type
    	(  declareResult
        $$ start
	$$ stubDebugCode debugStubs name
	$$ ppBlock (ccode $$ withSemi push)
        ) $$ access_stubs
 , proto $$ access_proto
 )
 where
    name'      = addPrimPrefix name
    proto      = text "extern" <+> c_type     <> semi
    proto_ghc  = text "extern" <+> c_type_stg <> semi
    c_type_stg = ret_type_stg   <+> 
                 ppSimpleApply name' ((map (arg_ghc) args) `sepdBy` comma)
    c_type = ret_type   <+> 
             ppSimpleApply name' ((map (arg) args) `sepdBy` comma)

    arg_ghc p =
        (text (kindToStgType (paramKind p))) <+> text (haskellName p)

    arg p = text (cType p) <+> text (haskellName p)
    {-
     C stub return either void, a prim type or a pointer to
     a structure holding multiple results.
    -}
    (ret_type,ret_type_stg) =
      case results of
	 []  -> let v = text "void" in (v,v)
	 [p] -> (text (cType p), text (kindToStgType (paramKind p)))
	 _  -> (text "void*", text "StgAddr")

    (declareResult, push, pop, access_stubs, access_proto_ghc, access_proto, _) = 
                                 resultPassing GHC_ccall ("","","") name' end results

ppCasm GHC_casm _ debugStubs (Casm name _ _ _ gcsafe start ccode end args results) =
  ( casm gcsafe
    	(  declareResult
        $$ start
    	$$ hsep (zipWith initArg args [(0::Int)..])
        $$ stubDebugCode debugStubs name
	$$ ppBlock (ccode $$ push )
	) (map (text.haskellName) args)
      $$ pop
  , empty
  , empty
  , empty
  , empty
  )
  where
    (declareResult, push, pop, _, _, _, _) =
           resultPassing GHC_casm ("","","") name end results

    initArg p n = ppAssign (cExpr p) (parens (text (cType p)) <> text ("%" ++ show n))


\end{code}

%************************************************************************
%*									*
\subsection{Code for constructing a casm}
%*									*
%************************************************************************

The only complication in printing the type is that we replace Stable
Pointers with type variables.  Fortunately, any type variables will do
(since the type var doesn't affect how we pack or unpack) so we just
use @a1@ ... @am@ for the arguments and @r1@ ... @rn@ for the results.

And the other complication is that we print ForeignObj's as Ptr's 
because they will have been marshalled by now.

\begin{code}

ppType :: Target -> [Kind] -> [Kind] -> Doc
ppType target args res 
  = (zipWith (kindToType target) atvs args ++ [r]) `sepdBy` text " -> "
   where
    atvs = [ 'a':show i | i <- [(1::Int)..] ]
    rtvs = [ 'r':show i | i <- [(1::Int)..] ]
    r    = 
      case target of { FFI -> (text "IO" <+>) ; _ -> id} $
      ppTuple (zipWith (kindToType target) rtvs res)

kindToType :: Target -> String -> Kind -> Doc
kindToType tgt tv k = 
  case k of
    StablePtr    -> text (show k) <+> text tv
    Ptr          -> text (show k) <+> text tv
    FunPtr       -> text (show k) <+> text tv
    ForeignPtr{} -> text "Ptr" <+> text tv
    Word Natural -> text "Word32"
    Word s    -> text ("Word" ++ showSize s)
    Int s     -> text ("Int" ++ showSize s)
    Foreign{} -> text "ForeignObj"
    _         -> text (show k)
\end{code}


\begin{code}

ppPrimStgDecl :: Doc -> Doc -> Doc
ppPrimStgDecl c_type body
    =  c_type
    $$ char '{'
    $$ indent body
    $$ char '}'
\end{code}

%************************************************************************
%*									*
\subsection{GHC code for returning multiple results from a casm}
%*									*
%************************************************************************

resultPassing decides which policy to use for returning results from C.
There are three possibilities:

\begin{enumerate}
\item Returning nothing is easy:

  casm ``...'' >> return ()

\item Returning one thing is easy too:

  casm ``...''

\item Returning many things is tricky.  We pack up all the bits into a
  struct and return a pointer to the struct.  We then read each returned
  value out of the struct.  Blech!

  casm ``static struct { int res1; float res2; } gc_result;
         ...;
         gc_result.res1 = res1;
         gc_result.res2 = res2;
         %r = &gc_result;'' >>= \ gc_result ->
  casm ``%r = (struct { int res1; float res2; }*)%0 -> x;'' (gc_result::Ptr) >>= \ x ->
  casm ``%r = (struct { int res1; float res2; }*)%0 -> y;'' (gc_result::Ptr) >>= \ y ->


\end{itemize}

\begin{code}

resultPassing :: Target	  -- Currently just GHC_{ccall,casm}
	      -> (String,String,String)
	      -> String	  -- function name
	      -> Doc	  -- cleanup code
	      -> [Param] 
	      -> ( Doc	  -- How to declare the var
                 , Doc	  -- How to save the vars in C
		 , Doc    -- C function stubs to fetch pieces of result.
		 , Doc    -- Prototypes for C function stubs 
		          --   (STG signatures.)
		 , Doc    -- Prototypes for C function stubs 
		          --   (`native' signatures.)
                 , Doc    -- How to get them back in Haskell
		 , Doc    -- Possible foreign decls for the access stubs.
		 )     
-- No vars
resultPassing _ _ _ end [] =
  ( empty
  , end
  , empty
  , empty
  , empty
  , empty
  , empty
  )

-- One var
resultPassing target _ _ end [(Param n _ e _ _ _)] =
 case target of
  GHC_ccall ->
    ( empty
    , end $$ ppSimpleApply "return" (text e)
    , textline [">>= \\ ", n, " ->"]
    , empty
    , empty
    , empty
    , empty
    )
  FFI ->
    ( empty
    , end $$ ppSimpleApply "return" (text e)
    , textline [">>= \\ ", n, " ->"]
    , empty
    , empty
    , empty
    , empty
    )
  GHC_casm ->
   ( empty
   , end $$ ppAssign "%r" (text e)
   , textline [">>= \\ ", n, " ->"]
   , empty
   , empty
   , empty
   , empty
   )
  _  -> error ("resultPassing: wrong target -- " ++ show target ++ 
               "; expected GHC or FFI.")

-- Many vars
resultPassing target (cconv,hfile,ext_nm) nm end xs =
 case target of 
  GHC_ccall ->
    ( text "static" <+> structTy <+> text "gc_result;"
    , vcatMap copyIn xs $$ end $$ return_result
    , text ">>= \\ gc_result ->" $$ vcatMap copyOut xs
    , vcatMap mkCopyOutStub xs
    , vcat protos_ghc
    , vcat protos
    , empty
    )
  FFI ->
    ( text "static" <+> structTy <+> text "gc_result;"
    , vcatMap copyIn xs $$ end $$ return_result
    , text ">>= \\ gc_result ->" $$ vcatMap copyOut xs
    , vcatMap mkCopyOutStub xs
    , vcat protos_ghc
    , vcat protos
    , vcat ffi_decls
    )
  GHC_casm ->
    ( text "static" <+> structTy <+> text "gc_result;"
    , vcatMap copyIn xs $$ end $$ return_result
    , text ">>= \\ gc_result ->" $$ vcatMap copyOut xs
    , empty
    , empty
    , empty
    , empty
    )
  _ -> error ("resultPassing: wrong target -- " ++ show target ++ "; expected GHC or FFI.")

 where
  (protos_ghc, protos, ffi_decls) = unzip3 $ map mkCopyOutProtos xs

  structTy = text "struct" <+> braces (semiList (map decl xs) <> semi)
   where
    decl :: Param -> Doc
    decl p = text (kindToCType target (paramKind p)) <+> text (haskellName p)

  return_result =
    case target of
      GHC_casm  -> text "%r = &gc_result;"
      GHC_ccall -> ppSimpleApply "return" (text "&gc_result")
      FFI       -> ppSimpleApply "return" (text "&gc_result")
      _         -> error ("resultPassing: wrong target -- " ++ show target ++ 
      			  "; expected GHC or FFI.")

  -- Copy a return value into the result structure.
  copyIn :: Param -> Doc
  copyIn p = ppAssign ("gc_result." ++ haskellName p) (text (cExpr p))

  -- Copy a return value out of the result structure.
  copyOut :: Param -> Doc
  copyOut p =
     let n = haskellName p in
     case target of 
      FFI ->
        text "access_" <> text nm <> text "_" <> text n <+>
	hsep [(text "gc_result")] <+> textline [">>= \\", n, "->"]
      GHC_ccall ->
        (ccall False{-not safe-}) (text "access_" <> text nm <> text "_" <> text n)
	[(text "(gc_result :: Addr)")] <+> textline [">>= \\", n, "->"]
      GHC_casm ->
	(casm False{-not safe-} (ppAssign "%r" ("%0" `deRef` n)) 
	         [text "(gc_result :: Addr)"])   <+> 
	textline [">>= \\", n, "->"]
      _        -> error ("resultPassing: wrong target -- " ++ show target ++ 
      		         "; expected GHC or FFI.")

  {-
    C_ret_type access_funNm_field(void *ptr) 
    { return ((struct _type)ptr)->field; }
  -}
  mkCopyOutStub p@Param{} =
    let n = haskellName p in
    text (cType p) <+> text (access_nm n) <> 
    text "(void *ptr){ return(" <> ("ptr" `deRef` n) <> text ");}"

  mkCopyOutProtos p@(Param n _ _ ct k _) =
   ( mkProto (cType p) (kindToCType target Ptr)
   , mkProto ct "void*"
   , text "foreign import" <+> text cconv <+> text "unsafe" <+> 
     ext_loc <+> text (access_nm n) <+> text "::" <+>
     ppType FFI [Ptr] [k]
   )
    where
     mkProto ret_ty arg_ty  =
      text "extern " <>  (text ret_ty) <+>
      text (access_nm n) <> text ('(':arg_ty++");")

  ext_loc = doubleQuotes (
      (text hfile) <+>
      (if null ext_nm then empty else brackets (text ext_nm)))

  access_nm n = "access_" ++ nm ++ '_':n

  deRef arg n = ppCast (structTy <> char '*') (text arg) <> 
                text "->" <> 
		text n

\end{code}

%************************************************************************
%*									*
\subsection{GHC Specific Utilities}
%*									*
%************************************************************************

\begin{code}

casm :: Bool -> Doc -> [Doc] -> Doc
casm safe d args =
  ((if safe then ("_casm_GC_ ``", "''") else ("_casm_ ``", "''"))
       `around`
   (ppBlock (indent d))) <+> hsep args

ccall :: Bool -> Doc -> [Doc] -> Doc
ccall safe d args = (text call <+> d) <+> hsep args
  where
   call | safe      = "_ccall_GC_"
        | otherwise = "_ccall_"

stubDebugCode :: Bool -> String -> Doc
stubDebugCode True nm = text ("__current_fun__ = " ++ show nm) <> semi
stubDebugCode _    _  = empty

addPrimPrefix :: String -> String
addPrimPrefix nm = "prim_" ++ nm
\end{code}

%************************************************************************
%*									*
\subsection{Tests}
%*									*
%************************************************************************

COMMENTED OUT.

\begin{code}
{-
tst t = putStrLn $ render (call $$ decl $$ entry $$ c)
 where
  (call, decl, entry, c) = ppCasm FFI False t

t1 = Casm 
       "foo"
       False
       (text "int x; void* q; char p; void* q")
       (text "p = foo(&q,x,y);")
       (text "free q")
       [ Param "x" "x" "int" Int    
       , Param "q" "q" "int *"  Addr] 
       [ Param "p" "'p'" "char" Char
       , Param "q" "q" "char *" Addr] 
-}
\end{code}

