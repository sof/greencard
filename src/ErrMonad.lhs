%
%
%

Simple propagation of (fatal) errors.

\begin{code}
module ErrMonad 

       (
         ErrM(..)        -- instances: Functor, Monad
       , failure         -- :: a -> ErrM a b
       , addErrorContext -- :: [a] -> ErrM [a] b -> ErrM [a] b
       , runErrM         -- :: ErrM a b  -> Either a b
       , runErrMs        -- :: [ErrM a b] -> ([a], [b])
       , catchFailure    -- :: ErrM a -> (String -> ErrM a) -> ErrM a

       , mapErrM	 -- :: (a -> b) -> ErrM a -> ErrM b
       ) where

import Control.Monad (ap)
\end{code}

\begin{code}
data ErrM a b
 = Failed a
 | Succeeded b

mapErrM :: (a->b) -> ErrM f a -> ErrM f b
mapErrM f e =
    case e of
      Failed    err -> Failed err
      Succeeded v   -> Succeeded (f v)

instance Functor (ErrM a) where
  fmap = mapErrM

instance Applicative (ErrM a) where
  pure = return
  (<*>) = ap

instance Monad (ErrM a) where
  (>>=) m f =
    case m of
      Failed err   -> Failed err
      Succeeded v  -> f v

  return v = Succeeded v
\end{code}

\begin{code}
failure :: a -> ErrM a b
failure err_msg = Failed err_msg

catchFailure :: ErrM a b -> (a -> ErrM a b) -> ErrM a b
catchFailure m f =
 case m of
  Failed err -> f err
  _          -> m

addErrorContext :: [a] -> ErrM [a] b -> ErrM [a] b
addErrorContext str m =
  case m of
    Failed err   -> Failed (str ++ err)
    Succeeded _  -> m

runErrMs :: [ErrM a b] -> ([a], [b])
runErrMs [] = ([],[])
runErrMs (x:xs) =
  let
   (as,bs) = runErrMs xs
  in
  case x of
    Failed e    -> (e:as,bs)
    Succeeded s -> (as,s:bs)

runErrM :: ErrM a b -> Either a b
runErrM m =
  case m of
    Failed err  -> Left err
    Succeeded v -> Right v
\end{code}
