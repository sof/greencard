%
% The Foo Project
%

Monad to support the fill in code:

\begin{code}
module FillInMonad
	( 
	  FilM
	, runFilM
	, getPrefixes	    -- :: FilM [String]
	, getDISEnv         -- :: FilM DISEnv
	, getTarget         -- :: FilM Target
	, getNameMangleFlag -- :: FilM Bool
	, addErrorContext   -- :: String -> FilM a -> FilM a
	, liftErrM	    -- :: ErrM a -> FilM a
        ) where

import qualified ErrMonad as EM
import DIS  (DISEnv)
import Target (Target)
import Control.Monad (ap)
\end{code}

\begin{code}
newtype FilM a
 = FilM (  DISEnv 
        -> [String]
	-> Target
	-> Bool
	-> EM.ErrM String a)

\end{code}

\begin{code}
runFilM :: DISEnv
	-> [String]
	-> Target
	-> Bool
	-> [FilM a]
	-> ([String], [a])
runFilM env pref trgt mangle films = 
  EM.runErrMs (map (\ (FilM act) -> act env pref trgt mangle) films)

addErrorContext :: String -> FilM a -> FilM a
addErrorContext str (FilM act) = 
 FilM (\ env pref tgt mangle -> EM.addErrorContext str (act env pref tgt mangle))

getDISEnv :: FilM DISEnv
getDISEnv = FilM (\ env _ _ _ -> return env)

getTarget :: FilM Target
getTarget = FilM (\ _ _ tgt _ -> return tgt)

getPrefixes :: FilM [String]
getPrefixes = FilM (\ _ pre _ _ -> return pre)

getNameMangleFlag :: FilM Bool
getNameMangleFlag = FilM (\ _ _ _ flg -> return flg)

liftErrM :: EM.ErrM String a -> FilM a
liftErrM em = FilM (\ _ _ _ _ -> em)
\end{code}

\begin{code}
thenFilM :: FilM a -> (a -> FilM b) -> FilM b
thenFilM (FilM act) cont = 
  FilM (\ env pref trgt m -> do
	 res <- act env pref trgt m
	 let (FilM act2) = cont res
	 act2 env pref trgt m)

returnFilM :: a -> FilM a
returnFilM v = FilM (\ _ _ _ _ -> return v)

instance Monad FilM where
  (>>=)  = thenFilM
  return = returnFilM

instance Applicative FilM where
  pure = return
  (<*>) = ap

instance Functor FilM where
  fmap f (FilM act) = FilM (\ env pre tgt m -> do
        		      v <- act env pre tgt m
        		      return (f v))
\end{code}
