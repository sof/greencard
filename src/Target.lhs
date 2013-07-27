%
% Copyright (C) 1997, 2003 Thomas Nordin and Alastair Reid
%

\begin{code}

module Target
	( Target(..)   -- instances: Eq, Show
	) where

data Target 
  = GHC_casm
  | GHC_ccall
  | FFI
    deriving (Eq) -- needed by Proc.genProc.mkResult

instance Show Target where
  showsPrec _ t =
   case t of
     GHC_ccall -> showString "GHC{-ccall-}"
     GHC_casm  -> showString "GHC{-casm-}"
     FFI       -> showString "FFI"

\end{code}
