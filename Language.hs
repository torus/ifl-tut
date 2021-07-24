
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

pprExpr (EAp e1 e2) = pprExpr e1 `iAppend` iStr " " `iAppend` pprAExpr e2

pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr " ", iIndent (pprDefns defns), iNewline
            , iStr "in ", pprExpr expr
            ]
    where
        keyword | not isrec = "let"
                | otherwise = "letrec"

pprExpr (ECase e as)
  = iStr "case" `iAppend` pprExpr e `iAppend` iConcat (map alter as)
    where
        alter (n, args, e) = iStr (show n) `iAppend` iInterleave (iStr " ") (map iStr args) `iAppend` pprExpr e

pprExpr (EConstr n m) = iStr "Pack{" `iAppend` iInterleave (iStr ",") [iStr (show n), iStr (show m)] `iAppend` iStr "}"
pprExpr (ELam as e) = iStr "\\" `iAppend` iInterleave (iStr " ") (map iStr as) `iAppend` iStr " . " `iAppend` pprExpr e



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
flatten col ((IStr s, indent) : seqs) = s ++ flatten col seqs
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

{-
>>> pprint sampleProgram
>>> iDisplay $ pprExpr $ (\(a,b,c) -> c) (head sampleProgram)
"main = double 21 ;\ndouble = x + x"
"double 21"
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

