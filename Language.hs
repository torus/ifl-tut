
{-
G マシン
TIM 比較的あたらしい
セミコロンがある
IO ない
数値

1.1  局所定義
let と letrec わかれる
let は関数定義できない。局所関数がかけない
最後にリフティングする

スーパーコンビネーター

パック
代数データ型

スーパーコンビネーターとは
コンビネーターは自由変数をもたない
スーパーコンビネーターは本体部分にでてくるラムダ抽象も自由変数をもたない

CAF は？
変数をもたないやつ
constant applicable form

1.2
演算子が不思議
割り算と引き算は括弧が必ず必要
1-2-3  とかかけない
(1-2)-3 と書く必要がある
+ と * は右結合
パーサ作るときは右結合の方が簡単
適用のみ左結合
& | は論理演算
Miranda 風

1.3
基本のデータは式
-}

module Language where
import Utils
import CoreStats (CoreStats(cs_jb))
import Data.Char

data Expr a
    = EVar Name
    | ENum Int
    | EConstr Int Int
    | EAp (Expr a) (Expr a)
    | ELet
        IsRec
        [(a, Expr a)]
        (Expr a)
    | ECase
        (Expr a)
        [Alter a]
    | ELam [a] (Expr a)
    deriving (Show)

type CoreExpr = Expr Name
type Name = String

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [ name | (name, rhs) <- defns]
{-
map fst とおなじ。内包表記がすき
-}

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [ rhs | (name, rhs) <- defns ]

type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

sampleProgram =
  [ ("main", [], EAp (EVar "double") (ENum 21))
  , ("double", ["x"], EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))
  , ( "sumDoubles"
    , ["x", "y"]
    , ELet nonRecursive [
        ("doubledX", EAp (EVar "double") (EVar "x"))
        , ("doubledY", EAp (EVar "double") (EVar "y"))
      ] (EAp (EAp (EVar "+") (EVar "doubledX")) (EVar "doubledY"))
    )
  ]


preludeDefs :: CoreProgram
preludeDefs =
    [ ("I", ["x"], EVar "x")
    , ("K", ["x", "y"], EVar "x")
    , ("K1", ["x", "y"], EVar "y")
    , ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                 (EAp (EVar "g") (EVar "x")))
    , ("compose", ["f", "g", "x"], EAp (EVar "f")
                                       (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ]

{-
1.5
>>> preludeDefs
[("I",["x"],EVar "x"),("K",["x","y"],EVar "x"),("K1",["x","y"],EVar "y"),("S",["f","g","x"],EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))),("compose",["f","g","x"],EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),("twice",["f"],EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))]
-}

pprint :: CoreProgram -> String

{-
pprExpr :: CoreExpr -> String
pprExpr (ENum n)    = show n
pprExpr (EVar v)    = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2
-}

{-
パターン足りてないけど今はこれだけ（どうせ使わない）
-}

pprAExpr :: CoreExpr -> Iseqrep
pprAExpr e
    | isAtomicExpr e = pprExpr e
    | otherwise      = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (replicate n e2)

{-
-- Ex 1.1
>>> pprExpr (mkMultiAp 30 (EVar "f") (EVar "x"))
"f x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x"

1.5.2
Iseq インターフェイスの I
Iseq は型クラスの方が良い

-}

class Iseq iseq where
    iNil     :: iseq
    iStr     :: String -> iseq
    iAppend  :: iseq -> iseq -> iseq
    iNewline :: iseq
    iIndent  :: iseq -> iseq
    iDisplay :: iseq -> String

data Iseqrep = INil
             | IStr String
             | IAppend Iseqrep Iseqrep
             | IIndent Iseqrep
             | INewline
             deriving (Show)

instance Iseq Iseqrep where
    iNil         = INil
    iStr         = IStr
    iAppend      = IAppend
    iDisplay seq = flatten 0 [(seq, 0)]
    iIndent seq  = IIndent seq
    iNewline     = INewline

pprExpr :: CoreExpr -> Iseqrep
pprExpr (ENum n)    = iStr (show n)
pprExpr (EVar v)    = iStr v

-- 1.5.5
pprExpr (EAp (EAp (EVar "+") e1) e2)
  = iConcat [ pprAExpr e1, iStr " + ", pprAExpr e2 ]

pprExpr (EAp e1 e2)
  = iConcat [ pprExpr e1
            , iStr " "
            , pprAExpr e2
            ]

pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword
            , iNewline
            , iStr "    "
            , iIndent (pprDefns defns)
            , iNewline
            , iStr "in "
            , pprExpr expr
            ]
    where
        keyword | not isrec = "let"
                | otherwise = "letrec"

pprExpr (ECase e as)
  = iConcat [ iStr "case "
            , pprExpr e
            , iStr " of"
            , iNewline
            , iStr "    "
            , iIndent $ iInterleave sep (map alter as)
            ]
    where
        sep = iConcat [ iStr " ;", iNewline]
        alter (n, args, e)
         = iConcat [ iStr "<", iStr (show n), iStr "> "
                   , iConcat $ map (\a -> iConcat [iStr a, iStr " "]) args
                   , iStr "-> "
                   , pprExpr e
                   ]

pprExpr (EConstr n m)
  = iConcat [ iStr "Pack{"
            , iInterleave (iStr ",") [iStr (show n), iStr (show m)]
            , iStr "}"]
pprExpr (ELam as e)
  = iConcat [ iStr "\\"
            , iInterleave (iStr " ") (map iStr as)
            , iStr " . "
            , pprExpr e
            ]

sampleProgramCase :: [([Char], [[Char]], Expr [Char])]
sampleProgramCase
  = [ ( "isRed", ["c"]
      , ECase (EVar "c") [ (1, [], EVar "True")
                         , (2, [], EVar "False")
                         , (3, [], EVar "False")
                         ])
    , ( "depth", ["t"]
      , ECase (EVar "t") [ (1, ["n"], ENum 0)
                         , (2, ["t1", "t2"]
                           , EAp (EAp (EVar "+") (ENum 1))
                              (EAp (EAp (EVar "max") (EAp (EVar "depth") (EVar "t1")))
                                   (EAp (EVar "depth") (EVar "t2"))))
                         ])
  ]

pprDefns :: [(Name, CoreExpr)] -> Iseqrep
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
      sep = iConcat [ iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> Iseqrep
pprDefn (name, expr)
    = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr)]

infixr 5 `iAppend`

iConcat :: Iseq iseq => [iseq] -> iseq
iInterleave :: Iseq iseq => iseq -> [iseq] -> iseq

{-
Ex. 1.2
-}

iConcat = foldr iAppend iNil
iInterleave sep [] = iNil
iInterleave sep [seq] = seq
iInterleave sep (seq : seqs) = seq `iAppend` (sep `iAppend` iInterleave sep seqs)

{-
>>> pprExpr $ EAp (EVar "f") (EVar "x")
IAppend (IStr "f") (IAppend (IStr " ") (IStr "x"))
-}

space n = replicate n ' '

flatten :: Int -> [(Iseqrep, Int)] -> String

flatten col [] = ""
flatten col ((INil, indent) : seqs) = flatten col seqs
flatten col ((IStr s, indent) : seqs) = s ++ flatten (col + length s) seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((INewline, indent) : seqs) = '\n' : (space indent ++ flatten indent seqs)
flatten col ((IIndent seq, indent) : seqs) = flatten col ((seq, col) : seqs)

pprint prog = iDisplay (pprProgram prog)

pprProgram :: CoreProgram -> Iseqrep
pprProgram [] = INil
pprProgram ds
  = iInterleave (iStr " ;" `iAppend` iNewline) $ map def ds
    where
        def (name, args, e) = iConcat [ iStr name, iStr " = ", pprExpr e]

test :: IO ()
test = do { putStrLn $ pprint sampleProgramCase }

{-
>>> test
-}

-- 1.5.6

iNum :: Int -> Iseqrep
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseqrep
iFWNum width n
  = iStr (space (width - length digits) ++ digits)
    where
        digits = show n

iLayn :: [Iseqrep] -> Iseqrep
iLayn seqs = iConcat (zipWith (curry lay_item) [1..] seqs)
    where
        lay_item (n, seq)
          = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline]

{- Layn の Mirand のプレリュードにあるらしい -}

-- 1.6

-- clex :: String -> [Token]
syntax :: [Token] -> CoreProgram
syntax = undefined

-- parse :: String -> CoreProgram
-- parse = syntax . clex

type Token = (Int, String)
--type Token = String

clex :: Int -> [Char] -> [Token]
clex n ('-' : '-' : cs) = clex n (dropWhile ('\n' /=) cs)
clex n (c:cs)
  | isWhiteSpace c = clex n cs
  | isDigit c      = (n, numToken) : clex n restCs'
  | isAlpha c      = (n, varToken) : clex n restCs
  | take 2 (c:cs) `elem` twoCharOps = (n, opToken) : clex n restCs''
  | otherwise      = (n, [c]) : clex n cs
      where
        (idCs, restCs)   = span isIdChar cs
        varToken         = c : idCs
        (numCs, restCs') = span isDigit cs
        numToken         = c : numCs
        (opToken, restCs'') = ([c, head cs], tail cs)
clex _ []            = []

{-
>>> clex "abc xyz 1bb == 123 /= x"
["abc","xyz","1","bb","==","123","/=","x"]
-}

type Loc = Int

{-
clex :: Loc -> String -> [Token]
clex i ('-' : '-' : cs) = clex i (dropWhile ('\n' /=) cs)
clex i (c1 : c2 : cs)
  | isJust (lookup [c1, c2] binOps) = (i, [c1, c2]) : clex i cs
clex i ('\n' : cs) = clex (succ i) cs
clex i (c : cs)
  | isDigit c = case span isDigit cs of
      (ds, rs) -> (i, c : ds) : clex i rs
  | isAlpha c = case span isIdChar cs of
      (vs, rs) -> (i, c : vs) : clex i rs
  | isSpace c = clex i cs
  | otherwise = (i, [c]) : clex i cs
clex _ "" = []
-}

isWhiteSpace :: Char -> Bool
isIdChar     :: Char -> Bool

isWhiteSpace c = c `elem` " \t\n"
isIdChar c     = isAlpha c || isDigit c || c == '_'

-- Ex. 1.10
twoCharOps :: [String]
twoCharOps = ["==", "/=", ">=", "<=", "->", "&&", "||"]

-- 1.6.2
{- エラーも返せる -}
type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
{-
pLit s ((_, tok) : toks)
  | s == tok = [(s, toks)]
  | otherwise = []
pLit _ []     = []
-}

pVar :: Parser String
{-
pVar [] = []
pVar ((_, tok) : toks) = case tok of
  c:_ | isAlpha c -> [(tok, toks)]
-}

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = pLit "hello" `pAlt` pLit "goodbye"

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks
                             , (v2, toks2) <- p2 toks1 ]

pGreeting :: Parser (String, String)
{-
pGreeting = pThen keepFirst
                  (pThen mkPair pHelloOrGoodbye pVar)
                  (pLit "!")
  where
    keepFirst = const
    mkPair    = (,)
-}

pGreeting = pThen3 mkGreeting
              pHelloOrGoodbye
              pVar
              (pLit "!")
            where
              mkGreeting hg name exlamation = (hg, name)

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 f pa pb pc toks
  = [ (f v1 v2 v3, toks2) | (v1, toks1) <- pa toks
                          , (v2, toks2) <- pb toks1
                          , (v3, toks3) <- pc toks2]

{-
>>> pGreeting [(0, "goodbye"), (0, "James"), (0, "!")]
[(("goodbye","James"),[(0,"!")])]
-}

{-
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []
-}

pEmpty :: a -> Parser a
pEmpty x toks = [(x, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

-- Ex. 1.14
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks
  = [ (f x, toks') | (x, toks') <- p toks]

(<$$>) :: (a -> b) -> Parser a -> Parser b
(<$$>) = flip pApply

(<**>) :: Parser (a -> b) -> Parser a -> Parser b
(<**>) = pap

(<**) :: Parser c -> Parser b -> Parser c
(<**) = pThen const

(**>) :: Parser a -> Parser c -> Parser c
(**>) = pThen (\_ y -> y)

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep
  = (:) <$$> p <**> pOneOrMore (sep **> p)
--  = pThen (:) p $ pOneOrMore (sep `op` p)
--    where
--      op = pThen (\_ a -> a)

pap :: Parser (a -> b) -> Parser a -> Parser b
pap pf pa toks = [ (f x, toks2) | (f, toks1) <- pf toks
                                , (x, toks2) <- pa toks1 ]

-- satisfy
pSat :: (String -> Bool) -> Parser String
pLit s = pSat (s ==)

-- Ex. 1.16
pSat pre toks = case toks of
  ((_, t) : toks') | pre t -> [(t, toks')]
  _                        -> []

-- Ex. 1.17
pVar = pSat (\tok -> isAlpha (head tok) && tok `notElem` keywords)

keywords :: [String]
keywords = ["let", "letrec", "in", "case", "of", "Pack"]

-- Ex. 1.18
pNum :: Parser Int
pNum = read <$$> pSat (all isDigit)

-- Ex. 1.19
{-
後ろにかならず失敗するパーサがあるとすべての候補を試すことになる。
>>> pOneOrMore (pLit "x") $ clex 1 "x x x x x"
[(["x","x","x","x","x"],[])]
-}

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAltL` pEmpty []

pAltL :: Parser a -> Parser a -> Parser a
pAltL p1 p2 toks = p1 toks <+ p2 toks
  where
    [] <+ ys = ys
    xs <+ _  = xs

infixr 5 `pAltL`
