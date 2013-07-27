%
% Copyright (C) 1997 Thomas Nordin and Alastair Reid
%

The data types used to represent the abstract syntax
tree for a Green Card specification. 

\begin{code}
module Decl
	( Decl(..)
	, Sig
	, Call
	, CCode
	, Fail
	, Result
	, showDecls

        , SrcLoc
        , mkSrcLoc
	, incSrcLineNo
        , noSrcLoc
        , setSrcLocFile
        , setSrcLocName

	) where

import Name ( Name )
import DIS  ( DIS  )
import Type ( Type )
\end{code}

\begin{code}
data Decl
  = ProcSpec Sig
	     (Maybe Call) 
	     (Maybe CCode) 
	     (Maybe Fail) 
	     (Maybe Result) 
	     (Maybe String)
  | Haskell String
  | C String
  | DisDef   Name [Name] DIS
  | Constant Type [(Name, Name)]   -- Haskell, C name
  | Enum 
      Name           -- name of enum type
      Type           -- type that enums map to.
      [Name]         -- derivings
      [(Name, Name)] -- Haskell, C name
  | Prefix  Name
  | Include Name 
  | DllName Name
  | CConv   Name
  deriving ( Show )
 
-- type signature
type Sig        = 
  ( SrcLoc
  , Name         -- Haskell name (modulo reqd. name mangling.)
  , Maybe Name   -- shorthand for C function
  , Maybe Type   -- the typesig's context.
  , Type         -- the real type to the right of => (the one *we* care about.)
  )

type Call       = [DIS]

type CCode      = ( Bool{-safe?-}
                  , String
		  )

type Fail       = [( String {- failure pred. -}
                   , String {- error handler -}
		   )]

type Result     = DIS

showDecls :: [Decl] -> String
showDecls ds = unlines (map show ds)

data SrcLoc
 = NoSrcLoc
 | SrcLoc 
     FilePath -- filename
     String   -- name
     Int      -- line number.

mkSrcLoc :: String{-filename-} -> Int{-line no.-} -> SrcLoc
mkSrcLoc nm l = SrcLoc nm "" l

incSrcLineNo :: SrcLoc -> SrcLoc
incSrcLineNo NoSrcLoc = NoSrcLoc
incSrcLineNo (SrcLoc path nm i) = SrcLoc path nm (i+1)

setSrcLocFile :: FilePath -> SrcLoc -> SrcLoc
setSrcLocFile _  NoSrcLoc = error "setSecLocFile: can't set location on NoSrcLoc"
setSrcLocFile md (SrcLoc _ nm l) = SrcLoc md nm l

setSrcLocName :: FilePath -> SrcLoc -> SrcLoc
setSrcLocName _  NoSrcLoc = error "setSecLocFile: can't set name on NoSrcLoc"
setSrcLocName nm (SrcLoc path _ l) = SrcLoc path nm l

noSrcLoc :: SrcLoc
noSrcLoc = NoSrcLoc

instance Show SrcLoc where
  showsPrec d loc =
    case loc of
     SrcLoc md nm l -> 
        showString (show md) .
        showString ", proc. spec " .
        showString (show nm) . 
        showString ", line " . 
	showsPrec d l
     NoSrcLoc  -> 
        showString "<unknown>, line <unknown>"
 
\end{code}
