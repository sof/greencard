%
%
%

As a temporary stop gap - a (hand written) lexer
for Green Card. Sigh.

\begin{code}
module Lex ( topLex, runLexer ) where

import Prelude hiding ( lex )
import GCToken
import LexM
import qualified Data.Map as M
import Casm     ( lookupKind )
import Decl

import Data.List( intersperse )
import Data.Char( isAlpha, isDigit, isLower, isSpace, isAlphaNum )

\end{code}

This lexer for Green Card is not particularly eventful.
The only interesting thing going on is that we may call
the lexer from two different parser `states': one for
lexing the contents of a Green Card directive, the other
for passing on Haskell code verbatim.

The @topLex@ action is the entry point for the parser,
and it takes care of invoking the right sub-state. (The
lexer moves back to into the `verbatim' state upon exit
from the lexing of a directive.)

It could be argued that a stream-based interface between lexer
and parser would solve this entering and exit from sub-states
more cleanly, but it comes at a cost (error recovery and the
passing of info such as line numbers is more expensive.)

\begin{code}
runLexer :: String -> LexM [Decl] -> String -> IO [Decl]
runLexer fname m str = runLexM fname str (setLexState 0{-lex-} >> m)

type LexCont a = Token -> LexM a
type Lexer a   = LexCont a -> LexM a

topLex :: Lexer a
topLex cont = do 
 eof <- isEOF
 if eof
  then cont T_eof
  else do
    {-
     We encode the different states that a lexer might be in
     using an Int in the lexer monad. Here, we fish it out
     and dispatch to the appropriate lexer.
     
     A better solution would be for the lexing action to
     be stored directly in the monad, but happy 1.9 (and later)
     no longer accommodates this. (The lexer now has to have the
     the general type (lexer :: (Token -> P a) -> P a); 'a' can
     no longer be substituted for the parser return type.
    -}
   lState <- getLexState
   case lState of
     0 -> lex cont
     1 -> lexDirect cont
     2 -> lexUserDIS cont
     _ -> error ("Lex.topLex: unknown state " ++ show lState)

\end{code}

Initial state of the lexer when starting on a new line.

\begin{code}
lex :: Lexer a
lex cont = do 
   c <- getNextChar
   case c of
    '%' -> -- looks directive-like this..
          lexDirective cont
    '{' -> do
      c1 <- getNextChar
      case c1 of
        '-' -> lex_nested_comment ["{-"] 1{-seen-} cont
	_   -> haskellCode [c,c1] cont
    _ -> do
      putBackChar c
      haskellCode [] cont
\end{code}

Gobble up a line of Haskell, taking care of dealing with strings
and comments correctly.

\begin{code}
haskellCode :: [Char] -> Lexer a
haskellCode as cont = do
  cs <- getStream
   -- mind-bogglingly quick this.
  case break (\x -> x == '{'
                 || x == '\n'
                 || x == '-'
                 || x == '\\'
		 || x == '"'{-"-} 
  	     ) cs of
   (ls,rs) ->
    case rs of     
     ('"'{-"-}:cs1) -> do
       let (str,cs2) = lex_string cs1
       setStream cs2
       haskellCode (as++ls++'"'{-"-}:str) cont
     ('\n':cs1) -> -- end of the line.
       incLineNo $ do
       setStream cs1
       cont (T_haskell (as++ls))
     ('\\':'"'{-"-}:cs1) -> do  -- escaped quote, eat it.
       setStream cs1
       haskellCode (as++"\\\"") cont
     ('{':'-':cs1) -> do  -- entering block comment.
       setStream cs1
       lex_nested_comment (["{-",as,ls]) 1{-seen-} cont
     ('-':'-':cs1) -> do  -- one-liner, spool until end of line.
       case break (=='\n') cs1 of
        (ls1,cs2) -> do
	   let cs3 = case cs2 of [] -> cs2; (_:cs4) -> cs4
	   setStream cs3
	   incLineNo $ do
	     cont (T_haskell (as++ls++'-':'-':ls1))
     (c:cs1) -> do
       setStream cs1
       haskellCode (as++ls++[c]) cont
     [] -> do
       setStream []
       cont (T_haskell (as++ls))
\end{code}

\begin{code}
lexDirective :: Lexer a
lexDirective cont = do
  c <- getNextChar
  case c of
   'C' -> cCode cont
   '-' -> cCode cont
   '{' -> codeChunk (\ code cont1 -> cont1 (T_ccode code)) cont
   '#' -> do
      cs <- getStream
      case span (isAlpha) cs of
       (xs,cs1)
         | xs /= "include" -> do
	     setStream cs1
	     cont (T_unknown xs)
         | otherwise -> do
	     let (fname,cs2) = break (=='\n') cs1
	     setStream (tail cs2)
	     incLineNo (cont (T_hinclude (dropWhile isSpace fname)))
   _ -> do      
      cs <- getStream
      case span (isAlpha) cs of
       (xs,cs1) -> do
            setStream cs1
       	    case M.lookup  (c:xs) kwordsFM of
	      Nothing    -> cont (T_unknown (c:xs))
	      Just contf -> do
	           setLexState 1{-lexDirect-}
		   contf cont

kwordsFM :: M.Map String (Lexer a)
kwordsFM = M.fromList $
 [ ("code",     codeChunk (\ code c -> c (T_ccode code)))
 , ("safecode", codeChunk (\ code c -> c (T_safecode code)))
 , ("end",      codeChunk (\ code c -> c (T_end code)))
 , ("prefix",   \ c -> c T_prefix)
 , ("fun",      \ c -> c T_fun)
 , ("call",     \ c -> c T_call)
 , ("result",   \ c -> c T_result)
 , ("fail",     \ c -> c T_fail)
 , ("const",    \ c -> c T_const)
 , ("enum",     \ c -> c T_enum)
 , ("dis",      \ c -> c T_dis)
 , ("dllname",  \ c -> c T_dllname)
 , ("callconv", \ c -> c T_callconv)
 ]
\end{code}

A code chunk consist of sequence of characters,
possibly spanning multiple lines. The 'continuation'
character on a new line is '%' in column 0.

\begin{code}
codeChunk :: (String -> Lexer a) -> Lexer a
codeChunk retcont cont = do
 cs <- getStream
 go [] cs
  where
   go chunk_acc ls = do
     (line_acc, res) <- go_line [] ls
     incLineNo (
       case res of
         Left the_end -> do
             setStream the_end
	     ret (line_acc:chunk_acc)
         Right rs -> do
             go (line_acc:chunk_acc) rs
       )

   go_line acc ls =
    case span (\x -> x /= '\n' && x /= '%') ls of
      (as,[]) -> return (acc++as, Left [])
      (as,'\n':'%':c1:cs1)
          | isSpace c1 -> return (acc++as, Right (c1:cs1))
 	  | otherwise  -> return (acc++as, Left  ('%':c1:cs1))
      (as,'\n':cs1)    -> return (acc++as, Left cs1)
      (as,'%':'}':cs1) -> return (acc++as, Left (tail (dropWhile (/='\n') cs1)))
      (as,'%':cs1)     -> go_line (acc++as++"%") cs1

   ret ccode = do
     let ccode' = concat (intersperse "\n" (reverse ccode))
     setLexState 0{-lex-}
     retcont ccode' cont
\end{code}

\begin{code}
lex_string :: String -> ({-the-}String, String)
lex_string cs = 
 case go [] cs of
   (acc, str) -> (concat (reverse acc), str)
 where
  go acc cs1 =
   case break (\x -> x == '\\' || x == '"'{-"-}) cs1 of
    (ls,[]) -> (ls:acc,[])
    (ls,'\\':'"'{-"-}:cs2) -> -- escaped quote
       go ("\\\"":ls:acc) cs2
    (ls,'\\':'\\':cs2) -> -- escaped slash
       go ("\\\\":ls:acc) cs2
    (ls,'\\':x:cs2) -> -- OK, so it doesn't cope with a file ending with a double slash
       go (['\\',x]:ls:acc) cs2
    (ls,'"'{-"-}:cs2) ->
       ("\"":ls:acc, cs2)
\end{code}

\begin{code}
lex_nested_comment :: [String] -> Int -> Lexer a
lex_nested_comment sofar cnt cont = go sofar cnt
 where
  go acc count = do
   cs <- getStream
   case span (\ x -> x /='{' && x /= '-' && x /= '\n') cs of
    (ls,rs) ->
     case rs of
      [] -> do
       setStream []
       cont (T_haskell (concat (reverse (ls:acc))))
      ('\n':cs1)   -> do
       setStream cs1
       incLineNo (go ("\n":ls:acc) count)
      ('{':'-':cs1) -> do
        setStream cs1
        go ("{-":ls:acc) (count+1)
      ('-':'}':cs1) -> do
        setStream cs1
	let acc' = ("-}":ls:acc)
        if count == 1 
	 then haskellCode (concat (reverse acc')) cont
	 else go acc' (count-1)
      (c:cs1)     -> do
         setStream cs1
         go ([c]:ls:acc) count

\end{code}

\begin{code}
lexDirect :: Lexer a
lexDirect cont = do
 cs <- getStream
 case dropWhile (\x -> isSpace x && x/='\n') cs of
  [] -> do
    setStream cs
    cont T_eof
  (c:cs1) -> do
   setStream cs1
   case c of
    '}' -> cont T_ccurly
    '(' -> cont T_oparen
    ')' -> cont T_cparen
    '[' -> cont T_osquare
    ']' -> cont T_csquare
    ',' -> cont T_comma
    '.' -> cont T_dot
    '"' -> cont T_dquote
    '=' -> do
      c1 <- getNextChar
      case c1 of
        '>' -> cont T_darrow
        _   -> do
          putBackChar c1
	  cont T_equal
    '<' -> do
      c1 <- getNextChar
      setLexState 2{-lexUserDIS-}
      case c1 of
       '<' -> cont T_odangle
       _   -> do
         putBackChar c1
	 cont T_oangle
    '>' -> do
      c1 <- getNextChar
      case c1 of
       '>' -> cont T_cdangle
       _   -> do
         putBackChar c1
	 cont T_cangle
    '-' -> do
      c1 <- getNextChar
      case c1 of
       '>' -> cont T_arrow
       '-' -> do
         cs2 <- getStream
	 case dropWhile (/='\n') cs2 of
	  ('\n':'%':c2:cs3)
	     |isSpace c2 ->
	         incLineNo $ do
		   setStream (c2:cs3)
		   lexDirect cont 
	     | otherwise -> do
	         incLineNo $ do
		   setLexState 0{-lex-}
		   setStream ('%':c2:cs3)
		   lex cont 
	  ('\n':cs3) ->  do
	         incLineNo $ do
		   setLexState 0{-lex-}
		   setStream cs3
		   lex cont 
	  [] -> cont T_eof
       _   -> do
         putBackChar c1
	 cont (T_unknown cs)
    ':' -> do
      c1 <- getNextChar
      case c1 of
       ':' -> cont T_dcolon
       _   -> do
         putBackChar c1
	 cont (T_unknown cs)
    '%' -> do
      c1 <- getNextChar
      case c1 of
       '%' -> do
         cs2 <- getStream
	 case span (isAlphaNum) cs2 of
	   (ls,cs3) -> do
	     setStream cs3
	     case lookupKind ls of
	       Nothing -> cont (T_unknown cs3)
	       Just k  -> cont (T_kind k)
       _   -> do
	 putBackChar c1
	 lexName (T_disname) c cont
     {-
      The syntax of greencard is currently ambiguous,

         Foo { res1 = y } 
	 
      could either mean the constructor Foo applied to the
      result of C expression "res1=y" or the record constructor
      Foo with the field res1 equal/set to y.
      
      To resolve the two, we insist on there not being any 
      whitespace between a record constructor and the obrace.
      (It's too late in the day for GC to start making
       deep-rooted syntax changes..)
     -}
    '{' -> do
      cExp cont 
    '\n' -> do
     catchEOF (cont T_eof) $
      incLineNo $ do
      c1 <- getNextChar
      case c1 of
       '%' -> do
         c2 <- getNextChar
         putBackChar c2
	 if isSpace c2 
	  then lexDirect cont
	  else do
            putBackChar c1
	    setLexState 0{-lex-}
	    lex cont
	    
       _   -> do
         putBackChar c1
	 setLexState 0{-lex-}
	 lex cont

    _ 
     | isLower c -> lexName (T_disname) c cont
     | otherwise -> lexName (T_name) c cont

lexName :: (String -> Token) -> Char -> Lexer a
lexName fo c cont = do
  cs <- getStream
  case span (\x -> isAlpha x    || 
		   isDigit x    || 
                   x == '\''    ||
		   x == '`'     ||
		   x == '-'     ||
		   x == '_') cs of
    (ls,rs) -> do
     let ls' = c:ls
     setStream rs
     case ls' of
      "declare" -> cont T_declare
      "in"      -> cont T_in
      _         -> 
        case rs of
	 ('{':rs1) -> do
	    setStream rs1
	    cont (T_reccon ls')
         [] -> cont (fo ls')
         _ -> cont (fo ls')

cCode :: Lexer a
cCode cont = do
 cs <- getStream
 go [] cs
  where
   go acc ls =
    case span (/= '\n') ls of
      (as,[]) -> do
         setStream []
	 ret  (as:acc)
      (as,'\n':'%':cs1) ->
         incLineNo (go (as:acc) cs1)
      (as,'\n':cs1) ->
         incLineNo $ do
         setStream cs1
	 ret (as:acc)

   ret ccode = 
       let ccode' = concat (intersperse "\n" (reverse ccode)) in
       cont (T_c ccode')

{- Note: isn't clever about nested braces nor strings. -}
cExp :: Lexer a
cExp cont = do
 cs <- getStream
 case span (/='}') cs of
  (ce,cs1) -> do
    let cs2 = case cs1 of [] -> [] ; (_:cs3) -> cs3
    setStream cs2
    cont (T_cexp ce)
\end{code}

\begin{code}
lexUserDIS :: Lexer a
lexUserDIS cont = do
  cs <- getStream
  case span (/='/') cs of
   (_,[]) -> do
      setStream []
      setLexState 0{-lex-}
      cont (T_unknown cs)
   (ms,'/':cs1) ->
      go ms cs1

 where
  go_one acc cs = 
    case span (\x -> x /= '/' && x /= '-' && x /= '>') cs of
     (us,[]) -> return (Left (concat (reverse (us:acc)), []))
     (us,'-':'>':cs1) -> go_one ("->":us:acc) cs1
     (us,'/':cs1) -> return (Left  (concat (reverse (us:acc)), cs1))
     (us,'>':cs1) -> return (Right (concat (reverse (us:acc)), cs1))
     
  go ms cs1 = do
    res <- go_one [] cs1
    case res of
     Right (us,cs2) -> do
        setStream ('>':cs2)
	setLexState 1{-lexDirect-}
	cont (T_user (ms,us,Nothing))
     Left  (us,cs2) -> do
        res1 <- go_one [] cs2
	case res1 of
	 Right (k,cs3) -> do
	   setStream ('>':cs3)
  	   setLexState 1{-lexDirect-}
	   case lookupKind k of
	      Nothing -> cont (T_unknown cs2)
	      Just k1 -> cont (T_user (ms, us, Just k1))
         Left (_,cs3) -> do
           setStream cs3
	   setLexState 1{-lexDirect-}
	   cont (T_unknown cs2)
\end{code}
