module Main where

main :: IO ()
main = putStrLn $ show test

test = runProg "x"

-- Core-language p. 17

data Expr a
  = EVar Name
  | ENum Int
  | Econstr Int Int
  | EAp (Expr a) (Expr a)
  | ELet
    IsRec
    [(a, Expr a)]
    (Expr a)
  | ECase
    (Expr a)
    [Alter a]
  | ELam [a] (Expr a)
--  deriving (Text)

type CoreExpr = Expr Name
    


type Program a = [ScDefn a]
type CoreProgram = Program Name
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name
type Name = String
type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

-- p. 21

preludeDefs :: CoreProgram
preludeDefs
  = [ ("I",  ["x"], EVar "x"),
      ("K",  ["x", "y"], EVar "x"),
      ("K1", ["x", "y"], EVar "y"),
      ("S",  ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                  (EAp (EVar "g") (EVar "x"))),
      ("compose", ["f", "g", "x"], EAp (EVar "f")
                                       (EAp (EVar "g") (EVar "x"))),
      ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]

-- p. 25

iConcat :: [Iseq] -> Iseq
iConcat [] = IStr ""
iConcat (a : as) = iAppend a $ iConcat as

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _     [] = IStr ""
iInterleave _     [a] = a
iInterleave delim (a : as) = a `iAppend` delim `iAppend` iInterleave delim as

iNil              = INil
iAppend seq1 seq2 = IAppend seq1 seq2
iStr str          = IStr str


-- p. 27

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

iIndent seq = IIndent seq
iNewline = INewline

flatten :: Int
  -> [(Iseq, Int)]
  -> String

flatten col ((INewline, indent) : seqs)
  = '\n' : (space indent) ++ (flatten indent seqs)

flatten col ((IIndent seq, indent) : seqs)
  = flatten col ((seq, col) : seqs)

iDisplay seq = flatten 0 [(seq, 0)]




space 0 = ""
space n = ' ' : space (n - 1)

-- p. 28

iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width  n
  = iStr (space (width - length digits) ++ digits)
    where
      digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1 ..] seqs))
  where
    lay_item (n, seq)
      = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]


-- p. 62

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where
    str = show addr

-- A p. 270

shownum n = show n
hd :: [a] -> a
hd = head
tl :: [a] -> [a]
tl = tail
zip2 :: [a] -> [b] -> [(a, b)]
zip2 = zip

hInitial :: Heap a
hAlloc   :: Heap a -> a -> (Heap a, Addr)
hUpdate  :: Heap a -> Addr -> a -> Heap a
hFree    :: Heap a -> Addr -> Heap a

type Heap a = (Int, [Int], [(Int, a)])
type Addr   = Int

hInitial                               = (0,         [1 ..],  [])
hAlloc  (size, (next : free), cts) n   = ((size + 1, free,    (next, n) : cts), next)
hUpdate (size, free,          cts) a n = (size,     free,     (a, n) : remove cts a)
hFree   (size, free,          cts) a   = (size - 1, a : free, remove cts a)

-- p. 271

hLookup    :: Heap a -> Addr -> a
hAddresses :: Heap a -> [Addr]
hSize      :: Heap a -> Int

hNull   :: Addr
hIsnull :: Addr -> Bool


-- p. 272

hLookup (size, free, cts) a
  = aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap"))

hAddresses (size, free, cts) = [addr | (addr, node) <- cts]

hSize (size, free, cts) = size

hNull = 0
hIsnull a = a == 0
showaddr a = "#" ++ shownum a

remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] a = error ("Attempt to update or free nonexistent address #" ++
                    shownum a)
remove ((a', n) : cts) a | a == a' = cts
                         | a /= a' = (a', n) : remove cts a

-- A.2 p. 272

type ASSOC a b = [(a, b)]

aLookup []            k' def = def
aLookup ((k, v) : bs) k' def | k == k' = v
                             | k /= k' = aLookup bs k' def

aDomain :: ASSOC a b -> [a]
aDomain alist = [key | (key, val) <- alist]

aRange :: ASSOC a b -> [b]
aRange alist = [val | (key, val) <- alist]

-- p. 156

intCode = []

-- 4.2
-- p. 158

runProg     :: [Char] -> [Char]
compile     :: CoreProgram -> TimState
eval        :: TimState -> [TimState]
showResults :: [TimState] -> [Char]

runProg = showResults . eval . compile . parse

fullRun :: [Char] -> [Char]
fullRun = showFullResults . eval . compile . parse

parse = undefined

data Instruction = Take Int
                 | Enter TimAMode
                 | Push TimAMode

data TimAMode = Arg Int
              | Label [Char]
              | Code [Instruction]
              | IntConst Int

-- p. 159

type TimState = ([Instruction],
                 FramePtr,
                 TimStack,
                 TimValueStack,
                 TimDump,
                 TimHeap,
                 CodeStore,
                 TimStats)

data FramePtr = FrameAddr Addr
              | FrameInt Int
              | FrameNull

type TimStack = [Closure]
type Closure = ([Instruction], FramePtr)

data TimValueStack = DummyTimValueStack
data TimDump = DummyTimDump

type TimHeap = Heap Frame

fAlloc  :: TimHeap -> [Closure] -> (TimHeap, FramePtr)
fGet    :: TimHeap -> FramePtr -> Int -> Closure
fUpdate :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
fList   :: Frame -> [Closure]

type Frame = [Closure]

fAlloc heap xs = (heap', FrameAddr addr)
  where
    (heap', addr) = hAlloc heap xs

fGet heap (FrameAddr addr) n = f !! (n - 1)
  where
    f = hLookup heap addr

fUpdate heap (FrameAddr addr) n closure
  = hUpdate heap addr new_frame
    where
      frame = hLookup heap addr
      new_frame = take (n - 1) frame ++ [closure] ++ drop n frame

fList f = f

type CodeStore = ASSOC Name [Instruction]

codeLookup :: CodeStore -> Name -> [Instruction]
codeLookup cstore l
  = aLookup cstore l (error ("Attempt to jump to unknown label "
                            ++ show l))

statInitial  :: TimStats
statIncSteps :: TimStats -> TimStats
statGetSteps :: TimStats -> Int

-- p. 161

type TimStats = Int
statInitial = 0
statIncSteps s = s + 1
statGetSteps s = s

compile program
  = ([Enter (Label "main")],
     FrameNull,
     initialArgStack,
     initialValueStack,
     initialDump,
     hInitial,
     compiled_code,
     statInitial)
  where
    sc_defs          = preludeDefs ++ program
    compiled_sc_defs = map (compileSC initial_env) sc_defs
    compiled_code    = compiled_sc_defs ++ compiledPrimitives
    initial_env      = [(name, Label name) | (name, args, body) <- sc_defs]
                       ++ [(name, Label name) | (name, code) <- compiledPrimitives]

initialArgStack = []

initialValueStack = DummyTimValueStack
initialDump = DummyTimDump

-- p. 162

compiledPrimitives = []

type TimCompilerEnv = [(Name, TimAMode)]

compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])
compileSC env (name, args, body)
  = (name, Take (length args) : instructions)
  where
    instructions = compileR body new_env
    new_env = (zip2 args (map Arg [1 ..])) ++ env

compileR :: CoreExpr -> TimCompilerEnv -> [Instruction]
compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR (EVar v)    env = [Enter (compileA (EVar v) env)]
compileR (ENum n)    env = [Enter (compileA (ENum n) env)]
compileR e           env = error "compileR: can't do this yet"

compileA :: CoreExpr -> TimCompilerEnv -> TimAMode
compileA (EVar v) env = aLookup env v (error ("Unknown variable " ++ v))
compileA (ENum n) env = IntConst n
compileA e        env = Code (compileR e env)

-- 4.2.4

eval state
  = state : rest_states where
  rest_states | timFinal state = []
              | otherwise      = eval next_state
  next_state = doAdmin (step state)

doAdmin state = applyToStats statIncSteps state

timFinal ([], frame, stack, vstack, dump, heap, cstore, stats) = True
timFinal state                                                 = False

applyToStats stats_fun (instr, frame, stack, vstack,
                        dump, heap, cstore, stats)
  = (instr, frame, stack, vstack, dump, heap, cstore, stats_fun stats)

step ((Take n : instr), fptr, stack, vstack, dump, heap, cstore, stats)
  | length stack >= n = (instr, fptr', drop n stack, vstack, dump, heap', cstore, stats)
  | otherwise         = error "Too few args for Take instruction"
    where (heap', fptr') = fAlloc heap (take n stack)

step ([Enter am], fptr, stack, vstack, dump, heap, cstore, stats)
  = (instr', fptr', stack, vstack, dump, heap, cstore, stats)
    where (instr', fptr') = amToClosure am fptr heap cstore

step ((Push am : instr), fptr, stack, vstack, dump, heap, cstore, stats)
  = (instr, fptr, amToClosure am fptr heap cstore : stack,
     vstack, dump, heap, cstore, stats)

amToClosure :: TimAMode -> FramePtr -> TimHeap -> CodeStore -> Closure
amToClosure (Arg n)      fptr heap cstore = fGet heap fptr n
amToClosure (Code il)    fptr heap cstore = (il, fptr)
amToClosure (Label l)    fptr heap cstore = (codeLookup cstore l, fptr)
amToClosure (IntConst n) fptr heap cstore = (intCode, FrameInt n)

-- 4.2.5 p. 164

showFullResults states
  = iDisplay (iConcat [
                 iStr "Supercombinator definitions", iNewline, iNewline,
                 showSCDefns first_state, iNewline, iNewline,
                 iStr "State transitions", iNewline,
                 iLayn (map showState states), iNewline, iNewline,
                 showStats (last states)
                 ])
  where
    (first_state : rest_states) = states

showResults states
  = iDisplay (iConcat [
                 showState last_state, iNewline, iNewline, showStats last_state
                 ])
  where last_state = last states

showSCDefns :: TimState -> Iseq
showSCDefns (instr, fptr, stack, vstack, dump, heap, cstore, stats)
  = iInterleave iNewline (map showSC cstore)

showSC :: (Name, [Instruction]) -> Iseq
showSC (name, il)
  = iConcat [
  iStr "Code for ", iStr name, iStr ":", iNewline,
  iStr "  ", showInstructions Full il, iNewline, iNewline
  ]

showState :: TimState -> Iseq
showState (instr, fptr, stack, vstack, dump, heap, cstore, stats)
  = iConcat [
  iStr "Code:  ", showInstructions Terse instr, iNewline,
  showFrame heap fptr,
  showStack stack,
  showValueStack vstack,
  showDump dump,
  iNewline
  ]

-- p. 165

showFrame :: TimHeap -> FramePtr -> Iseq
showFrame heap FrameNull = iStr "Null frame ptr" `iAppend` iNewline
showFrame heap (FrameAddr addr)
  = iConcat [
  iStr "Frame: <",
  iIndent (iInterleave iNewline
           (map showClosure (fList (hLookup heap addr)))),
  iStr ">", iNewline
  ]
showFrame heap (FrameInt n)
  = iConcat [ iStr "Frame ptr (int): ", iNum n, iNewline ]

showStack :: TimStack -> Iseq
showStack stack
  = iConcat [ iStr "Arg stack: [",
              iIndent (iInterleave iNewline (map showClosure stack)),
              iStr "]", iNewline
            ]

showValueStack :: TimValueStack -> Iseq
showValueStack vstack = iNil

showDump :: TimDump -> Iseq
showDump dump = iNil

showClosure :: Closure -> Iseq
showClosure (i, f)
  = iConcat [ iStr "(", showInstructions Terse i, iStr ", ",
              showFramePtr f, iStr ")"
            ]

showFramePtr :: FramePtr -> Iseq
showFramePtr FrameNull = iStr "null"
showFramePtr (FrameAddr a) = iStr (show a)
showFramePtr (FrameInt n) = iStr "int " `iAppend` iNum n

showStats :: TimState -> Iseq
showStats (instr, fptr, stack, vstack, dump, heap, code, stats)
  = iConcat [ iStr "Steps taken = ", iNum (statGetSteps stats), iNewline,
              iStr "No of frames allocated = ", iNum (hSize heap),
              iNewline
            ]

-- p. 166

data HowMuchToPring = Full | Terse | None

showInstructions :: HowMuchToPring -> [Instruction] -> Iseq
showInstructions None il = iStr "{..}"
showInstructions Terse il
  = iConcat [iStr "{", iIndent (iInterleave (iStr ", ") body), iStr "}"]
    where
      instrs = map (showInstruction None) il
      body | length il <= nTerse = instrs
           | otherwise           = (take nTerse instrs) ++ [iStr ".."]
showInstructions Full il
  = iConcat [iStr "{ ", iIndent (iInterleave sep instrs), iStr " }"]
    where
      sep = iStr "," `iAppend` iNewline
      instrs = map (showInstruction Full) il

showInstruction d (Take m)  = (iStr "Take ")  `iAppend` (iNum m)
showInstruction d (Enter x) = (iStr "Enter ") `iAppend` (showArg d x)
showInstruction d (Push x)  = (iStr "Push ")  `iAppend` (showArg d x)

showArg d (Arg m)      = (iStr "Arg ")     `iAppend` (iNum m)
showArg d (Code il)    = (iStr "Code ")    `iAppend` (showInstructions d il)
showArg d (Label s)    = (iStr "Label ")   `iAppend` (iStr s)
showArg d (IntConst n) = (iStr "IntConst") `iAppend` (iNum n)

nTerse = 3

