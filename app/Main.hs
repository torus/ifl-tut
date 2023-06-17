module Main where

main :: IO ()
main = putStrLn $ show test

test = aEval (aCompile (Plus (Num 3) (Num 5)), [])

data AExpr = Num Int
           | Plus AExpr AExpr
           | Mult AExpr AExpr

aInterpret :: AExpr -> Int
aInterpret (Num n)      = n
aInterpret (Plus e1 e2) = aInterpret e1 + aInterpret e2
aInterpret (Mult e1 e2) = aInterpret e1 * aInterpret e2

data AInstruction = INum Int
                  | IPlus
                  | IMult

aEval :: ([AInstruction], [Int]) -> Int
aEval ([],          [n])         = n
aEval (INum n : is, s)           = aEval (is, n : s)
aEval (IPlus  : is, n0 : n1 : s) = aEval (is, n1 + n0 : s)
aEval (IMult  : is, n0 : n1 : s) = aEval (is, n1 * n0 : s)

aCompile :: AExpr -> [AInstruction]
aCompile (Num n)      = [INum n]
aCompile (Plus e1 e2) = aCompile e1 ++ aCompile e2 ++ [IPlus]
aCompile (Mult e1 e2) = aCompile e1 ++ aCompile e2 ++ [IMult]

-- TODO: Ex 3.2 handle let

