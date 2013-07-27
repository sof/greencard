%
% Copyright (C) 1997 Thomas Nordin and Alastair Reid
%

\begin{code}

module NameSupply
	( NameSupply	
        , NSM		
        , getNewName	-- :: NSM Name
        , getNewNames	-- :: Int -> NSM [Name]
        , initNS		
        , runNS
	, nameSupply	
	) where

import Name( Name )

\end{code}

%************************************************************************
%*									*
\subsection{Monadic plumbing for Name Supply}
%*									*
%************************************************************************

\begin{code}

type NameSupply = [Name]

newtype NSM a = NSM (NameSupply -> (a, NameSupply))

{- Try to do without this one for now - Haskell compatibility pitfall.
instance Functor NSM where
  map f (NSM g) = NSM (\ns -> let (a, ns') = g ns 
                              in  (f a, ns'))
-}

instance Monad NSM where
  (NSM f) >>= g	= 
    NSM (\ns -> let (result1, ns1)	= f ns
                    (NSM h)		= g result1 
                in h ns1)
  return a	= NSM (\ns -> (a, ns))

getNewNames :: Int -> NSM [Name]
getNewNames i = NSM (\ns -> splitAt i ns)

getNewName :: NSM Name
getNewName = NSM (\ns -> (head ns, tail ns))

initNS :: NSM a -> NameSupply -> (a, NameSupply)
initNS (NSM f) ns = f ns

runNS :: NSM a -> Name -> a
runNS (NSM f) n = fst (f (nameSupply n))

-- A good source of names, functions depend on this particular
-- implementations so don't change unless you know what you are doing.

nameSupply :: Name -> NameSupply
nameSupply foo = [foo ++ show n | n <- [(1::Int) ..]]

\end{code}
