%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%

The @LexM@ hides the information maintained by the Green Card lexer

\begin{code}
module LexM

       (
         LexM

       , runLexM     -- :: String -> LexM a -> IO a
       , ioToLexM    -- :: IO a   -> LexM a
       , incLineNo   -- :: LexM a -> LexM a
       , getSrcLoc   -- :: LexM SrcLoc
       , isEOF       -- :: LexM Bool
       , catchEOF    -- :: LexM a -> LexM a
       , getNextChar -- :: LexM Char
       , putBackChar -- :: Char -> LexM ()
       , getStream   -- :: LexM String
       , setStream   -- :: String -> LexM ()
       , getLineNo   -- :: LexM LineNo
       , getLexState -- :: LexM Int
       , setLexState -- :: Int -> LexM ()

       , thenLexM
       , returnLexM
       ) where

import Decl

import System.IO.Error ( isEOFError, ioeGetErrorString )
import qualified Control.Exception  ( catch )
import Data.List ( isSuffixOf )

import Control.Monad (ap)

-- components threaded by the monad (apart from
-- the IO token.)
data LexState
 = LexState
      SrcLoc
      Int        {- lex state -}
      String     {- input stream -}

newtype LexM a = LexM (  LexState -> IO (a, LexState))

runLexM :: String
        -> String
        -> LexM a
	-> IO a
runLexM fname str (LexM m) = do
  (v, _) <- m (LexState (mkSrcLoc fname 1) 0 str)
  return v

ioToLexM :: IO a -> LexM a
ioToLexM act =
 LexM (\ st -> do
         v <- act
	 return (v, st))

incLineNo :: LexM a -> LexM a
incLineNo (LexM m) =
 LexM (\ (LexState loc a b) -> m (LexState (incSrcLineNo loc) a b))

getSrcLoc :: LexM SrcLoc
getSrcLoc = LexM (\ st@(LexState loc _ _) -> return (loc, st))

isEOF :: LexM Bool
isEOF = LexM (\ st@(LexState _ _ cs) -> return (null cs, st))

catchEOF :: LexM a -> LexM a -> LexM a
catchEOF (LexM cont) (LexM m) =
  LexM (\ st -> Control.Exception.catch (m st) (handleIOError $ cont st))
 where
  handleIOError handler ex
    | isEOFErr ex = handler
    | otherwise   = ioError ex

  isEOFErr err
   = isEOFError err ||
     "EOF" `isSuffixOf` (ioeGetErrorString err) ||
      -- workaround ghc-5.04 ioeGetErrorString oddity.
     "\"EOF\"" `isSuffixOf` (ioeGetErrorString err)


getNextChar :: LexM Char
getNextChar =
  LexM (\ (LexState loc lst str) ->
      case str of
       []     -> ioError (userError "EOF")
       (c:cs) -> return (c, LexState loc lst cs))

putBackChar :: Char -> LexM ()
putBackChar c =
  LexM ( \ (LexState loc lst cs) ->
           return ((), LexState loc lst (c:cs)))

getStream :: LexM String
getStream = LexM (\ st@(LexState _ _ cs) -> return (cs, st))

setStream :: String -> LexM ()
setStream cs = LexM (\ (LexState loc lst _) -> return ((), LexState loc lst cs))

getLineNo :: LexM SrcLoc
getLineNo = LexM (\ st@(LexState loc _ _) -> return (loc, st))

getLexState :: LexM Int
getLexState = LexM (\ st@(LexState _ lst _) -> return (lst, st))

setLexState :: Int -> LexM ()
setLexState lState = LexM (\ (LexState l _ str) -> return ((), LexState l lState str))

-----

instance Functor LexM where
  fmap f (LexM m) = LexM $ \st -> do
    (a, st') <- m st
    return (f a, st')

instance Applicative LexM where
  pure = return
  (<*>) = ap

instance Monad LexM where
  (>>=)  = thenLexM
  return = returnLexM

thenLexM :: LexM a -> (a -> LexM b) -> LexM b
thenLexM (LexM m) n =
 LexM ( \ st -> do
          (a, st1) <- m st
	  let (LexM act) = n a
	  act st1 )

returnLexM :: a -> LexM a
returnLexM v = LexM (\ st -> return (v, st) )

\end{code}
