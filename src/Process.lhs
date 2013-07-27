%
% Copyright (C) 1997, 2001-2003 Thomas Nordin and Alastair Reid
%

Toplevel module that schedules the various stages
of the translation.

\begin{code}
module Process
	( processFile
	) where

import Data.List ( intersperse, sortBy, unzip4 )
import Data.Maybe( catMaybes, fromMaybe, mapMaybe )
import ListUtils ( prefix, dropSuffix, elemBy,
                   mkHaskellVar, lowerName, decons,
		   mplusMaybe, basename
		 )
import Control.Monad( when )
import Data.Char( isAlphaNum )
import Text.PrettyPrint
import PrettyUtils( renderLn, vcatMap )
import System.IO( hPutStrLn, stderr )

import Parse  ( gcParse  )
import Lex    ( runLexer  )
import Decl   ( Decl(..), showDecls )
import Name   ( Name )
import DIS    ( DIS(..), default_stdDIS )
import Proc   ( genProcs )
import FillIn ( fillInDecls )
import Target ( Target(..) )
import Type   ( ppType )
\end{code}

%************************************************************************
%*									*
\subsection{Processing a file}
%*									*
%************************************************************************

@processFile@ drives the translation, and is crying out for the use
of a mechanism to hide away from view all its arguments.

\begin{code}
processFile :: Target 
            -> Bool 
	    -> Bool 
	    -> Bool 
	    -> Bool 
	    -> Bool
	    -> Bool
	    -> (String,String)
	    -> [String]
	    -> [String] 
	    -> String 
	    -> String 
	    -> String 
	    -> IO ()
processFile target debug debugStub verbose safe forH14 mangle loc path exts file hfile cfile = do
  mbrawdecls <- tryRead verbose path exts file
  case mbrawdecls of
   Nothing       -> ioError (userError ("Can't read file " ++ file))
   Just rawdecls -> do
    disDefs     <- getDISdefs verbose rawdecls path exts file
    let

      includes = [ i | Include i <- rawdecls ]
      prefixes = sortBy lengthCmp $ "" : [n | Prefix n <- rawdecls]
       where
        lengthCmp x y = compare (length y) (length x) 

    emit debug "Parsed" (showDecls rawdecls)
--    emit debug "Protos" (render (vsepMap ppProtoProc protoProcs))
--    emit debug "Consts" (render (vsepMap ppProtoProc constProcs))
    let
     mod_name = basename (dropSuffix hfile)
     (fillin_errs, decls)  = fillInDecls disDefs prefixes target mangle rawdecls 
     (marshall_errs, code) = genProcs target safe debugStub forH14 loc mod_name decls

    case fillin_errs ++ marshall_errs of
     errs@(_:_) -> ioError (userError (unlines errs))
     _ -> do
--       emit debug "Expanded proto procs" (render (vsepMap ppProc procs'))
       let (hcode, ccode, entries, headers) = unzip4 code
       case target of
        GHC_casm -> do
         writeFile hfile haskell
         (if null c then return () else writeFile cfile c)
         emit debug "Haskell output" haskell
         emit debug "C output" c
          where
          c       = renderLn (vcat ccode)
          haskell = unlines (["{-# OPTIONS -#include " ++ s ++ " #-}"
                             | s <- includes 
                             ])  ++
                    renderLn (vcat hcode)

        GHC_ccall -> do
         writeFile hfile haskell
         writeFile cfile c
         writeFile ((dropSuffix cfile) ++ ".h")  protos
         writeFile hc_hfile  protos_ghc
         emit debug "Haskell output" haskell
         emit debug "C output" c
          where
          hc_hfile = (dropSuffix cfile) ++ "_ghc.h"
          haskell = unlines (["{-# OPTIONS -#include " ++ s ++ " #-}"
                             | s <- (includes ++ ['"':hc_hfile ++ "\""{-"-}])
                             ]) ++
                    renderLn (vcat hcode)
          c       = renderLn (ppHeader includes $$
                              vcat ccode)
          protos     = renderLn (vcat entries)
          protos_ghc = renderLn (vcat headers)

        FFI -> do
         when verbose $ do
           hPutStrLn stderr ("Writing file: " ++ show hfile)
           hPutStrLn stderr ("Writing file: " ++ show cfile)
           hPutStrLn stderr ("Writing file: " ++ show hc_hfile_ffi)
         writeFile hfile haskell
	 writeFile cfile c
         writeFile hc_hfile_ffi protos_ffi
         emit debug "Haskell output" haskell
         emit debug "C output" c
          where
          hc_hfile_ffi = (dropSuffix cfile) ++ ".h"
          haskell = renderLn (vcat hcode) -- add newline at the end.
          c       = 
	        renderLn (ppHeader (includes ++ [show hc_hfile_ffi]) $$
	  	          vcat ccode')
          protos_ffi = renderLn (  text "#ifndef" <+> text prot_symb
                                $$ text "#define" <+> text prot_symb
                                $$ vcat [ text "#include" <+> text s
                                        | s <- includes ++ ["\"HsFFI.h\""]
                                        ]
                                $$ vcat headers'
                                $$ text "#endif"
                                )

          -- multi-include protection
          prot_symb = "_" ++ mkCFname hc_hfile_ffi

	  ccode'   = filter (not.isEmpty) ccode
	  headers' = filter (not.isEmpty) headers

\end{code}

Turn a filename into a legal cpp identifier

\begin{code}

mkCFname :: FilePath -> String
mkCFname = map fixChar
 where
  fixChar c
    | isAlphaNum c = c
    | c `elem` "_" = c
    | otherwise    = '_'

\end{code}

Printing out C declarations and data structures.

\begin{code}

ppHeader :: [String] -> Doc
ppHeader includes =
  vcatMap text [ "#include " ++ i | i <- includes ]

\end{code}



\begin{code}

emit :: Bool -> String -> String -> IO ()
emit False _      _  = return ()
emit True  header xs = do
  hPutStrLn stderr ("\n\n*** " ++ header ++ " ***")
  hPutStrLn stderr xs

\end{code}

%************************************************************************
%*									*
\subsection{Collecting DIS definitions}
%*									*
%************************************************************************

Collecting all the DIS definitions from all readable files on the
import graph.

\begin{code}

getDISdefs :: Bool -> [Decl] -> [String] -> [String] -> String -> IO [(Name, ([Name], DIS))]
getDISdefs verbose decls path exts file = do
  let files = getDeclImports decls
  imports <- chaseImports verbose path exts files [(file,decls)]
  emit verbose "Imports" (show (map fst imports))
  let defs = concatMap ((mapMaybe isDissy).snd) imports

      isDissy (DisDef nm args dis) = Just (nm, (args, dis))
	-- create a straightforward (user) DIS on-the-fly for %enums.
      isDissy (Enum nm ty _ _)     
       = let
	  -- %dis nm x = <marshall_nm/unmarshall_nm> (int x)
          args = ["x"]
	  dis  = Apply (UserDIS False Nothing ("marshall_"++nm) ("unmarshall_"++nm))
		       [Apply (Var (lowerName (show (ppType ty)))) [Var "x"]]
         in 
	 Just (mkHaskellVar nm, (args, dis))
      isDissy _ = Nothing

  emit verbose "DIS definitions"  (unlines (map show defs))
  return defs

\end{code}

%************************************************************************
%*									*
\subsection{Chasing Imports}
%*									*
%************************************************************************

Chase a set of possibly recursive module imports maintaining a list of
files to try and a list of files that have been found. Returns the
names of the modules imported plus their declarations.

\begin{code}

type Imports = [(String,[Decl])]

chaseImports :: Bool -> [String] -> [String] -> [String] -> Imports -> IO Imports
chaseImports _ _ _ [] seen 
  = return (reverse seen) -- local decls take precedence over imported ones.

chaseImports verbose path exts (file:files) seen 
  | alreadySeen = chaseImports verbose path exts files seen
  | otherwise   = do
      (imports,decls) <- getImports verbose path exts file
      -- putStrLn (concat $ ["File ", file, " imports: "] ++ imports)
      chaseImports verbose path exts (imports ++ files) ((file,decls):seen)
    where
     alreadySeen = elemBy (\ (n,_) -> n == file) seen

getImports :: Bool -> [String] -> [String] -> String -> IO ([String], [Decl])
getImports verbose path exts file = do
  mb_decls <- tryRead verbose path exts file
  let decls = fromMaybe [] mb_decls 
  return (getDeclImports decls, decls)

getDeclImports :: [Decl] -> [String]
getDeclImports decls = catMaybes [mbImportName s | Haskell s <- decls]
\end{code}
      
\begin{code}

tryRead :: Bool -> [String] -> [String] -> String -> IO (Maybe [Decl])
tryRead verbose path exts name = do
  res <- doUntil (mbReadFile verbose)
                 (allFileNames path name exts)
  maybe sorry frontEnd res
 where 
  frontEnd ls = do
    v <- runLexer name gcParse ls
    return (Just v)

  sorry | name == "Foreign.GreenCard" = do
           hPutStrLn stderr ("Warning: unable to find Foreign.GreenCard along the import path: " ++
                             (concat (intersperse ", " path)))
	   hPutStrLn stderr ("  using built-in defaults instead.")
	   frontEnd default_stdDIS
        | otherwise =
	   return Nothing

doUntil :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe b)
doUntil _ [] = return Nothing
doUntil f (a:as) = do
  v <- f a
  case v of
   Nothing -> doUntil f as
   Just _  -> return v

mbImportName :: String -> Maybe String
mbImportName xs = maybe Nothing (Just . head) (iq `mplusMaybe` i)
  where
    iq	= prefix ["import", "qualified"] wxs
    i	= prefix ["import"] wxs
    wxs = mergeModuleName (tokens xs)

tokens :: String -> [String]
tokens s = case lex s of
           [] -> []
           [("","")] -> []
           ((t,s'):_) -> t : tokens s'

mergeModuleName :: [String] -> [String]
mergeModuleName (x:".":y:rest) = mergeModuleName ((x ++ "." ++ y) : rest)
mergeModuleName (x:      rest) = x : mergeModuleName rest
mergeModuleName []             = []

\end{code}

All filenames with prefix from @path@ and suffix from @exts@.

\begin{code}

allFileNames :: [String] -> String -> [String] -> [String]
allFileNames path file exts 
  = [addSuffix '/' d ++ f ++ (prefixWith '.' ext) | d <- path, f <- deHierarchialize file, ext <- exts]
    where
     addSuffix _  []  = []
     addSuffix ch ls  = 
        case (decons ls) of
	  (_,x)
	    | x == ch   -> ls
	    | otherwise -> ls++[ch]

     prefixWith _  [] = []
     prefixWith ch ls@(x:_)
       | ch == x   = ls
       | otherwise = ch:ls

\end{code}

All the obvious ways of converting a hierarchial filename to a
non-hierarchial filename.

\begin{code}

deHierarchialize :: FilePath -> [FilePath]
deHierarchialize f 
  | '.' `elem` f = [f, subst '.' '/' f, subst '.' '\\' f]
  | otherwise    = [f]
 where
  subst x y = map (\ c -> if c == x then y else c)

\end{code}

Try reading a file:

\begin{code}

mbReadFile :: Bool -> String -> IO (Maybe String)
mbReadFile verbose name = 
  catch 
   ( do
      ls <- readFile name
      if verbose 
       then hPutStrLn stderr ("Reading file: " ++ show name)
       else return ()
      return (Just ls))
   (const (return Nothing))

\end{code}
