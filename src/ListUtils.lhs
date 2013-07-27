\begin{code}

module ListUtils
	( 
	  prefix
	, split
        , dropSuffix
	, basename
	, insertIfMissing
	, elemBy
	, decons
	, escapeChar
	
	, haskelliseName
	, lowerName
	, upperName
	, mkHaskellCon
	, mkHaskellVar
	
	, equalLength

	, mplusMaybe
	) where

import Data.Char   ( toLower, toUpper, isUpper, isDigit )
import Data.Maybe  ( fromMaybe ) 

\end{code}

\begin{code}
prefix :: Eq a => [a] -> [a] -> Maybe [a] -- what's left
prefix [] ls = Just ls
prefix _  [] = Nothing
prefix (x:xs) (y:ys)
  | x == y    = prefix xs ys
  | otherwise = Nothing

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split a as = 
 case break (==a) as of
   (xs,[])   -> [xs]
   (xs,_:ys) -> xs:split a ys

\end{code}

Not a generic list function, per se..

\begin{code}
-- Dropping the extension off of a filename:
dropSuffix :: String -> String
dropSuffix str = 
 case dropWhile (\ch -> ch /= '.' && ch /= '/' && ch /= '\\' ) 
                (reverse str) of
      ('.':rs) -> reverse rs
      _        -> str
      -- give up if we reach a separator (/ or \) or end of list.

basename :: String -> String
basename str = go str str
 where
    -- bi-lingual, the upshot of which is that
    -- / isn't allowed in DOS-style paths (and vice
    -- versa \ isn't allowed in POSIX(?) style pathnames).
  go acc []	   = acc
  go _   ('/':xs)  = go  xs xs
  go _   ('\\':xs) = go  xs xs
  go acc (_:xs)	   = go acc xs


insertIfMissing :: Eq a => a -> [a] -> [a]
insertIfMissing el xs@[x] 
  | x == el   = xs
  | otherwise = [x,el]
insertIfMissing el (x:xs) 
  = x : insertIfMissing el xs
insertIfMissing _ [] = []
\end{code}

For reasons unknown not provided the standard List interface..

\begin{code}
elemBy :: (a -> Bool) -> [a] -> Bool
elemBy _       []	=  False
elemBy isEqual (y:ys)	=  isEqual y || elemBy isEqual ys
\end{code}

\begin{code}
decons :: [a] -> ([a],a)
decons [] = error "decons: empty list"
decons xs = go xs
 where
  go []  = error "decons: empty list"
  go [x] = ([], x)
  go (y:ys) = let (ls, l) = go ys in (y:ls, l)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Name Mangling}
%*                                                                      *
%************************************************************************

Convert a Type name to a DIS name

\begin{code}

lowerName :: String -> String
lowerName [] = []
lowerName (c:cs) = toLower c : cs

upperName :: String -> String
upperName [] = []
upperName (c:cs) = toUpper c : cs

-- From: THIS_IS_A_SILLY_ID, ThisIs_ANOTHER_Silly_ID
-- to: ThisIsASillyId, ThisIsAnotherSillyId
casifyName :: String -> String
casifyName nm = concatMap caseWord (split '_' nm)

caseWord :: String -> String
caseWord []     = []
caseWord (c:cs) = toUpper c : cs'
  where
   cs'
    | all (\ ch -> isUpper ch || isDigit ch) cs = map toLower cs
    | otherwise      = cs
\end{code}

Convert C name to Haskell name by stripping prefixes and 
converting first letter to lowercase.

\begin{code}
mkHaskellCon :: String -> String
mkHaskellCon = upperName

mkHaskellVar :: String -> String
mkHaskellVar = lowerName

haskelliseNameStd :: [String] -> String -> String
haskelliseNameStd ps n = lowerName (stripPrefixes ps n)

haskelliseNameClassic :: [String] -> String -> String
haskelliseNameClassic ps n = casifyName (stripPrefixes ps n)

stripPrefixes :: [String] -> String -> String
stripPrefixes ps n = 
   fromMaybe n $ 
   foldr (mplusMaybe) Nothing [ prefix p n | p <- ps ]

\end{code}

\begin{code}
haskelliseName :: Bool -> [String] -> String -> String
haskelliseName std ps n
 | std       = haskelliseNameStd ps n
 | otherwise = haskelliseNameClassic ps n
\end{code}

Escape a character within a string by prefixing it with another:

\begin{code}
escapeChar :: Char -> Char -> String -> String
escapeChar escChar pref str = go str
  where
   go [] = []
   go (x:xs) | x == escChar = pref:x:go xs
             | otherwise    = x:go xs
\end{code}

Yeah, right - a list utility. Defined to avoid
pointless Haskell 98 incompats.

\begin{code}
mplusMaybe :: Maybe a -> Maybe a -> Maybe a
mplusMaybe x@(Just _) _ = x
mplusMaybe _	      x = x
\end{code}

\begin{code}
equalLength :: [a] -> [b] -> Bool
equalLength [] [] = True
equalLength (_:xs) (_:ys) = equalLength xs ys
equalLength _  _  = False
\end{code}
