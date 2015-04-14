%
% Copyright (C) 1997 Thomas Nordin and Alastair Reid
%

\begin{code}

module MarshallMonad
	( MarshallM  -- instances : Functor, Monad

        , failWith
        , initMarshallM
        , runMarshallM

        , getNewNames	
        , getNewName
	, getSrcLoc
        , setDISContext
        , getDISContext
        , getTarget
	
	, mapMarshallM

	) where

import Name ( Name )
import qualified NameSupply
import Decl ( SrcLoc )
import DIS  ( DIS )
import ErrMonad
import Target (Target)
import Control.Monad (ap)

\end{code}

%************************************************************************
%*									*
\subsection{Monadic plumbing for Name Supply}
%*									*
%************************************************************************

An (env + state + exception) monad.

\begin{code}
newtype MarshallM a = 
  MarshallM
    (  MarshallMEnv
    -> MarshallMState
    -> ErrM String (a, MarshallMState))

data MarshallMEnv
 = MarshallMEnv {
      ma_dis       :: DIS,
      ma_srcloc    :: SrcLoc,
      ma_target    :: Target
   }

data MarshallMState
 = MarshallMState {
      ma_ns        :: NameSupply.NameSupply
   }

mapMarshallM :: (a -> b) -> MarshallM a -> MarshallM b
mapMarshallM f (MarshallM g) = 
     MarshallM
      (\ env st ->
          case g env st of
            Failed err        -> Failed err
	    Succeeded (v,st') -> 
                return (f v, st'))

instance Functor MarshallM where
   fmap = mapMarshallM

instance Applicative MarshallM where
  pure = return
  (<*>) = ap

instance Monad MarshallM where
  (MarshallM f) >>= g	= 
    MarshallM 
         (\ env st -> do
           case f env st of
	     Failed err        -> Failed err
	     Succeeded (v,st') ->
                let MarshallM h = g v in
	        h env st')
  return a = MarshallM (\ _env st -> Succeeded (a,st))

getNewNames :: Int -> MarshallM [Name]
getNewNames i = 
  MarshallM (\ _ st -> 
     let
      (vs,ns) = NameSupply.initNS (NameSupply.getNewNames i) (ma_ns st)
     in
     return (vs,st{ma_ns=ns}))

getNewName :: MarshallM Name
getNewName = 
  MarshallM ( \ _ st -> 
     let
      (vs,ns) = NameSupply.initNS (NameSupply.getNewName) (ma_ns st)
     in
     return (vs, st{ma_ns=ns}))

getSrcLoc  :: MarshallM SrcLoc
getSrcLoc = MarshallM (\ env st -> return (ma_srcloc env,st))

setDISContext :: DIS -> MarshallM a -> MarshallM a
setDISContext dis (MarshallM h) = MarshallM ( \ env st -> h env{ma_dis=dis} st)

getDISContext :: MarshallM DIS
getDISContext = MarshallM (\ env st -> return (ma_dis env,st))

getTarget :: MarshallM Target
getTarget = MarshallM (\ env st -> return (ma_target env,st))

failWith :: String -> MarshallM a
failWith str = MarshallM ( \ _ _ -> Failed str)

initMarshallM :: MarshallM a 
              -> SrcLoc 
	      -> Target
	      -> NameSupply.NameSupply 
	      -> Either String  (a, NameSupply.NameSupply)
initMarshallM (MarshallM f) loc t ns = 
  case f env st of
    Failed err  -> Left err
    Succeeded (r,st') -> Right (r,ma_ns st')
 where
  env = MarshallMEnv (error "DIS context not instantiated") loc t
  st  = MarshallMState ns

runMarshallM :: MarshallM a -> SrcLoc -> Target -> Name -> ErrM String a
runMarshallM (MarshallM f) loc t n = mapErrM (fst) (f env st)
 where
  env = MarshallMEnv (error "DIS context not instantiated") loc t
  st  = MarshallMState 	(NameSupply.nameSupply n)
\end{code}
