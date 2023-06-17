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

-- 3.3 Mark 1

runProg :: [Char] -> [Char]
runProg = showResults . eval . compile . parse

type GmState
  = (GmCode,    -- Current instruction stream
     GmStack,   -- Current stack
     GmHeap,    -- Heap of nodes
     GmGlobals, -- Global addresses in heap
     GmStats)   -- Statistics

type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode (i, stack, heap, globals, stats) = i

putCode :: GmCode -> GmState -> GmState
putCode i' (i, stack, heap, globals, stats)
  = (i', stack, heap, globals, stats)

data Instruction
  = Unwind
  | Pushglobal Name
  | Pushint Int
  | Push Int
  | Mkap
  | Slide Int

instance Eq Instruction
  where
    Unwind       == Unwind       = True
    Pushglobal a == Pushglobal b = a == b
    Pushint a    == Pushint b    = a == b
    Push a       == Push b       = a == b
    Mkap         == Mkap         = True
    Slide a      == Slide b      = a == b
    _            == _            = False

type GmStack = [Addr]

getStack :: GmState -> GmStack
getStack (i, stack, heap, globals, stats) = stack

putStack :: GmStack -> GmState -> GmState
putStack stack' (i, stack, heap, globals, stats)
  = (i, stack', heap, globals, stats)

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
getHeap (i, stack, heap, globals, stats) = heap

putHeap :: GmHeap -> GmState -> GmState
putHeap heap' (i, stack, heap, globals, stats)
  = (i, stack, heap', globals, stats)

data Node
  = NNum Int           -- Numbers
  | NAp Addr Addr      -- Applications
  | NGlobal Int GmCode -- Globals

type GmGlobals = ASSOC Name Addr

getGlobals :: GmState -> GmGlobals
getGlobals (i, stack, heap, globals, stats) = globals

statInitial  :: GmStats
statIncSteps :: GmStats -> GmStats
statGetSteps :: GmStats -> Int

type GmStats = Int
statInitial    = 0
statIncSteps s = s + 1
statGetSteps s = s

getStats :: GmState -> GmStats
getStats (i, stack, heap, globals, stats) = stats

putStats :: GmStats -> GmState -> GmState
putStats stats' (i, stack, heap, globals, stats)
  = (i, stack, heap, globals, stats')

--

type Name = String
type Addr = Int
type Heap a = (Int, [Int], [(Int, a)])
type ASSOC a b = [(a, b)]

--

eval :: GmState -> [GmState]
eval state = state : resetStates
             where
               resetStates | gmFinal state = []
                           | otherwise     = eval nextState
               nextState   = doAdmin (step state)

doAdmin :: GmState -> GmState
doAdmin s = putStats (statIncSteps (getStats s)) s

gmFinal :: GmState -> Bool
gmFinal s = case (getCode s) of
              []        -> True
              otherwise -> False

