%
% Copyright (C) 1997, 2003 Thomas Nordin and Alastair Reid
%
\begin{code}

module PrettyUtils
	( renderLn
	, textline	
        , indent	
        , around	
	, joinedBy, sepdBy, vsepdBy
	, vsep, hsepMap, hcatMap, vsepMap, vcatMap
        , commaList, semiList, withSemi
	, ppParen

	-- Haskell constructs
	, ppTuple, ppList, ppApply, ppSimpleApply, ppRecord
	, ppBind, ppReturn, ppText
	, ppLambda, ppCase, ppCases, ppIf, ppLet

	-- C constructs
	, ppCIf, ppAssign, ppCDeclare, ppCDecl, ppCast, ppBlock, ppStruct
	) where

import Text.PrettyPrint
import Data.Maybe( maybeToList )

\end{code}

Make sure we a final newline is added.

\begin{code}
renderLn :: Doc -> String
renderLn d = render (d $$ text "")
\end{code}

\begin{code}

textline :: [String] -> Doc
textline = hsep . map text

indent :: Doc -> Doc
indent = nest 2

around :: (String, String) -> Doc -> Doc
(a, b) `around` d =  text a <> d <> text b

joinedBy :: [Doc] -> (Doc -> Doc -> Doc) -> Doc
[] `joinedBy` _  = empty
xs `joinedBy` sp = foldr1 sp xs

sepdBy :: [Doc] -> Doc -> Doc
ds `sepdBy` sp = hcat (punctuate sp ds)

vsepdBy :: [Doc] -> Doc -> Doc
ds `vsepdBy` sp = vcat (punctuate sp ds)

hsepMap, hcatMap, vcatMap, vsepMap :: (a -> Doc) -> [a] -> Doc
hsepMap pp xs = hsep (map pp xs)
hcatMap pp xs = hcat (map pp xs)
vcatMap pp xs = vcat (map pp xs)
vsepMap pp xs = vsep (map pp xs)

vsep :: [Doc] -> Doc 
vsep ds = ds `joinedBy` ($+$)

--($+$) :: Doc -> Doc -> Doc
--d1 $+$ d2 = d1 $$ text "" $$ d2

commaList :: [Doc] -> Doc
commaList ds = ds `sepdBy` comma

semiList :: [Doc] -> Doc
semiList ds = ds `sepdBy` semi

vsemiList :: [Doc] -> Doc
vsemiList ds = ds `vsepdBy` semi

ppTuple :: [Doc] -> Doc
ppTuple ds = parens (commaList ds)

ppList :: Doc -> Doc -> [Doc] -> Doc
ppList sp = foldr (\a as -> a <> sp <> as)

ppApply :: Doc -> [Doc] -> Doc
ppApply d ds = ppParen (not (null ds)) ((d:ds) `sepdBy` space)

ppSimpleApply :: String -> Doc -> Doc
ppSimpleApply f e = text f <> parens e

ppRecord :: Doc -> [Doc] -> [Doc] -> Doc
ppRecord c fs vs 
  = c <> braces (commaList (zipWith (\f v -> f <> equals <> v) fs vs))

withSemi :: Doc -> Doc
withSemi d | isEmpty d = d
           | otherwise = d <> semi

-- inspired by Prelude.showParen
ppParen :: Bool -> Doc -> Doc
ppParen True  = parens
ppParen False = id

ppBind :: Doc -> (Doc, Doc) -> Doc
ppBind m (pat, k) = m <> text " >>= \\ " <> pat <> text " ->" $$ k

ppReturn :: Doc -> Doc
ppReturn x = ppApply (text "return") [parens x]

ppText :: String -> Doc
ppText "" = empty
ppText s  = text s

\end{code}

\begin{code}

ppLambda :: [Doc] -> (Doc -> Doc)
ppLambda vs b = text "\\" <+> (vs `sepdBy` space) <+> text "->" <+> b

ppCase :: Doc -> Doc -> (Doc -> Doc)
ppCase v d b = text "case" <+> v <+> text "of {" <+> d <+> text "->" $$ b <> text "}"

ppLet :: String -> Doc -> Doc
ppLet v e = 
  text "let " <> 
  text v      <+> 
  equals      <+> 
  e           <+>
  text "in"   

ppIf :: Doc -> Doc -> Doc -> Doc
ppIf cond t e 
  =  text "if" <+> cond
  $$ text "then" <+> t
  $$ text "else" <+> e

\end{code}

\begin{code}

ppCases :: Doc -> [(Doc, Doc)] -> Doc
ppCases discr alts = 
  (hang (text "case" <+> discr <+> text "of" <+> lbrace)
    3   (vsemiList [ pat <+> text "->" <+> body | (pat, body) <- alts ])) $$
  rbrace

\end{code}

Print:

  if (..) {..} 
  else if (..) {..}
  ..
  else if (..) {..}
  else {..}

\begin{code}

ppCIf :: [(Doc,Doc)] -> Maybe Doc -> Doc
ppCIf xs mbe = (map mkIf xs ++ map braces (maybeToList mbe)) `joinedBy` mkElse
 where
  mkIf (c,s) = text "if" <+> parens c <+> braces s
  d1 `mkElse` d2 = d1 $$ text "else" <+> d2
                  
\end{code}

Print a C assignment, C declaration (with initialisation), C cast, ...

\begin{code}

ppAssign :: String -> Doc -> Doc
ppAssign lhs rhs = text lhs <+> equals <+> rhs <> semi

ppCDeclare :: Doc -> Doc -> Doc -> Doc
ppCDeclare ty var is = ty <+> var <+> equals <+> is <> semi

ppCDecl :: String -> String -> Doc
ppCDecl t n = text t <+> text n <> semi

ppCast :: Doc -> Doc -> Doc
ppCast ty expr = parens (parens ty <+> expr)

ppBlock :: Doc -> Doc
ppBlock d = ("do {", "} while(0);") `around` (indent d)

ppStruct :: [Doc] -> Doc
ppStruct ds = braces (commaList ds)

\end{code}
