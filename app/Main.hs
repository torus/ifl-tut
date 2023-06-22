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

-- p. 97

step :: GmState -> GmState
step state = dispatch i (putCode is state)
             where (i : is) = getCode state

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Push n)       = push n
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind

pushglobal :: Name -> GmState -> GmState
pushglobal f state
  = putStack (a : getStack state) state
  where a = aLookup (getGlobals state) f (error ("Undeclared global " ++ f))

-- p. 98

pushint :: Int -> GmState -> GmState
pushint n state
  = putHeap heap' (putStack (a : getStack state) state)
  where (heap', a) = hAlloc (getHeap state) (NNum n)

mkap :: GmState -> GmState
mkap state
  = putHeap heap' (putStack (a : as') state)
  where (heap', a)      = hAlloc (getHeap state) (NAp a1 a2)
        (a1 : a2 : as') = getStack state

push :: Int -> GmState -> GmState
push n state
  = putStack (a : as) state
  where as = getStack state
        a  = getArg (hLookup (getHeap state) (as !! (n + 1)))

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2

-- p. 99

slide :: Int -> GmState -> GmState
slide n state
  = putStack (a : drop n as) state
  where (a : as) = getStack state

unwind :: GmState -> GmState
unwind state
  = newState (hLookup heap a)
  where
    (a : as) = getStack state
    heap     = getHeap state
    newState (NNum n)    = state
    newState (NAp a1 a2) = putCode [Unwind] (putStack (a1 : a : as) state)
    newState (NGlobal n c)
      | length as < n = error "Unwinding with too few arguments"
      | otherwise     = putCode c state

-- p. 270 A.1 The heap type

hInitial :: Heap a
hAlloc   :: Heap a -> a -> (Heap a, Addr)
hUpdate  :: Heap a -> Addr -> a -> Heap a
hFree    :: Heap a -> Addr -> Heap a
