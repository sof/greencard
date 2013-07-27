%
% Copyright (C) 1997-2003 Thomas Nordin and Alastair Reid
%

\begin{code}
module Main(main, main2, runGreenCard) where

import Package

import System.Console.GetOpt
import Process( processFile )
import Target( Target(..) )
import ListUtils( split, dropSuffix )

import System.Environment  ( getArgs )
import Data.Char    ( toLower )
import Data.Maybe   ( fromMaybe, listToMaybe )
import Control.Monad ( when )

\end{code}


%************************************************************************
%*									*
\subsection{Main program}
%*									*
%************************************************************************


Driver code.

\begin{code}
main :: IO ()
main = 
  do argv <- getArgs
     greencard (getOpt Permute options argv)

-- Entry point when using interactively
runGreenCard :: String -> IO ()
runGreenCard str = greencard (getOpt Permute options (words str))

{- Interpreter test harness -}
main2 :: String -> IO ()
main2 str = greencard (getOpt Permute options (words str))

{- test harness
tstg :: String -> IO ()
tstg file = main2 (unwords [file, "--include-dir=.", "--target=ghc"])
tstf file = main2 (unwords [file, "--debug", "--include-dir=.", "--target=ffi"])
-}

greencard :: ([Options],[String],[String]) -> IO ()
greencard (opts, optfiles, errs) = do
  when optPLibdir (putStrLn (fromMaybe "no --libdir= specificed" optLibdir))
  when optVersion (putStrLn version_msg)
  when optHelp	  (putStrLn usageMsg)
  case errs of
     [] ->
        case optfiles of
          [fname] -> 
            case targets of
              []         -> greencard' FFI fname -- default to ffi target
              ["ffi"]    -> greencard' FFI fname
              ["ghc"]    -> greencard' (if optNoInline then GHC_ccall else GHC_casm) fname
              _          -> putStrLn ("Unrecognised target: " ++ (head targets) ++ 
                                      targets_supported_msg)
          _ -> putStrLn usageMsg
     _ -> putStrLn (concat errs ++ usageMsg)
  where
    targets_supported_msg = " ( `ffi' and `ghc' recognised.)"

    usageMsg = usageInfo usageHeader options

    greencard' target fname =
      processFile target 
		  optDebug
		  optStubDebug 
		  optVerbose 
		  optSafeCode
		  optHaskell14
		  optMangle
		  (optCallConv, optDllName)
		  includedirs
		  suffixes
		  fname
		  ofile
		  ocfile
      where
       ofile = 
        case optofiles of 
          (x:_) -> x
	  _ -> (case optoprefix of
	         (x:_) -> x
		 _     -> (dropSuffix fname)) ++ ".hs"

       ocfile = 
        case optocfiles of 
          (x:_) -> x
	  _ -> (case optoprefix of
	         (x:_) -> x
		 _     -> (dropSuffix fname)) ++ "_stub_ffi.c"

    -- predicates for grabbing various on/off options:n
    optVersion	 = any (DumpVersion==)	opts
    optHelp	 = any (DumpHelp==)	opts
    optDebug	 = any (DumpDebug==)	opts
    optPLibdir	 = any (DumpLibdir==)	opts
    optStubDebug = any (OptStubDebug==)	opts
    optVerbose	 = any (DumpVerbose==)	opts
    optSafeCode	 = any (OptSafeCode==)	opts
    optNoInline  = any (OptNoInline==)	opts
    optHaskell14 = any (OptHaskell14==) opts

    targets        = [ map toLower t | OptTarget t <- opts ]
    optoprefix     = [pre | OptOutputPrefix pre <- opts] 
    optofiles      = [o   | OptOutputFile o     <- opts] 
    optocfiles     = [oc  | OptOutputCFile oc   <- opts] 
    optDllName	   = unwords [dl  | OptDllName dl <- opts]
    optCallConv    = fromMaybe defCallConv (listToMaybe [ cc  | OptCallConv cc <- opts])
    optLibdir      = listToMaybe [ dir | OptLibdir dir <- reverse opts]
                     
    defLibdir      = error "Library directory (--libdir) must be specified"

{- BEGIN_GHC_ONLY
#if __GLASGOW_HASKELL__ <= 401
    defCallConv = "_ccall"
#else
    defCallConv = "ccall"
#endif
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
    defCallConv = "ccall"
{- END_NOT_FOR_GHC -}

   {-
    Why not use simple #defines instead? Because we want the source to
    be processable without having a pre-processor present.
   -}
    includedirs = maybe id (\ x -> (x:)) optLibdir (
{- BEGIN_GHC_ONLY
#if PURE_WIN32
      (concat [split ';' d | OptIncludeDirs d <- reverse opts]) ++
#else
      (concat [split ':' d | OptIncludeDirs d <- reverse opts]) ++
#endif
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
      (concat [split ':' d | OptIncludeDirs d <- reverse opts]) ++
{- END_NOT_FOR_GHC -}
      default_paths)
    suffixes    = 
      (concat [split ':' s | OptSuffix s <- reverse opts]) ++ 
      default_suffixes

    optMangle = 
       case [ m | OptNameMangle m <- opts] of
	 ("classic":_) -> False
         _ -> True  -- i.e., std in the event you haven't
		    --  - supplied it
		    --  - misspelled it.

default_paths :: [String]
default_paths = [""]

default_suffixes :: [String]
default_suffixes = ["gc",""]
\end{code}

The command-line options recognised by Green Card.

\begin{code}

version_msg :: String
version_msg = 
 unlines
 [ name ++ ", version " ++ version
 , ""
 , "Report bugs to <alastair@reid-consulting-uk.ltd.uk> or <sof@galois.com>"
 ]

usageHeader :: String
usageHeader   = 
 unlines
 [ "Usage: greencard [OPTION]... SOURCE"
 , ""
 , "Run Green Card, a foreign function interface preprocessor"
 , "for Haskell, over SOURCE"
 , ""
 , "Green Card home page: "
 , "   http://www.haskell.org/greencard"
 ]

options :: [OptDescr Options]
options = [
   Option ['?','h'] ["help"] (NoArg DumpHelp)
      "print out this help message and exit",
   Option ['V'] ["version"] (NoArg DumpVersion)
      "output version information and exit",
   Option ['v'] ["verbose"] (NoArg DumpVerbose)
      "print more information",
   Option ['d'] ["debug"] (NoArg DumpDebug)
      "output extra debug information",
   Option []    ["print-libdir"] (NoArg DumpLibdir)
      "output default library location",
   Option []    ["libdir"] (ReqArg OptLibdir "DIR")
      "use base library location",
   Option ['i'] ["include-dir"] (ReqArg OptIncludeDirs "DIRS")
      "Add DIRS (colon separated) to the include search path",
   Option ['s'] ["suffix"] (ReqArg OptSuffix "SUFS")
      "colon separated list of source file suffixes",
   Option ['t'] ["target"] (ReqArg OptTarget "TARGET")
      "generate Haskell code for a particular system (ghc, ffi)",
   Option ['S'] ["safe-code"] (NoArg OptSafeCode)
      "call C `safely' (GHC only)",
   Option ['n'] ["no-inline"] (NoArg OptNoInline)
      "put C code in a separate file (GHC only)",
   Option ['4'] ["haskell1.4"] (NoArg OptHaskell14)
      "generate code compatible with a Haskell 1.4 system",
   Option ['m'] ["name-mangling-scheme"] (ReqArg OptNameMangle "SCHEME")
      "controls mapping of external names into Haskell ones (std, classic)",
   Option ['D'] ["stub-debug"] (NoArg OptStubDebug)
      "include debugging code in generated code",
   Option ['f'] ["dllname"] (ReqArg OptDllName "DLL")
      "generate 'foreign imports' to dynamic library DLL (FFI backend only)",
   Option ['C'] ["callconv"] (ReqArg OptCallConv "CCONV")
      "use specific calling convention (stdcall, ccall)",
   Option ['o'] ["output"] (ReqArg OptOutputFile "FILE")
      "write Green Card Haskell output to FILE",
   Option ['c'] ["output-c"] (ReqArg OptOutputCFile "FILE")
      "write Green Card C output to FILE",
   Option ['p'] ["output-prefix"] (ReqArg OptOutputPrefix "PRE")
      "write Green Card Haskell output to PRE.{hs,c}"]

data Options
 = DumpHelp
 | DumpVersion
 | DumpVerbose
 | DumpDebug
 | DumpLibdir
 | OptLibdir String
 | OptIncludeDirs String
 | OptSuffix String
 | OptTarget String
 | OptSafeCode
 | OptNoInline
 | OptHaskell14
 | OptNameMangle String
 | OptStubDebug
 | OptDllName  String
 | OptCallConv String
 | OptOutputFile String
 | OptOutputCFile String
 | OptOutputPrefix String
   deriving ( Eq )

\end{code}
