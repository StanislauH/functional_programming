data Command 
    = Push Int          
    | Pop               
    | Add               
    | Sub              
    | Mul             
    | Div              
    | Jump Int         
    | JumpIfZero Int   
    | Output          
    deriving (Show, Eq)

type Stack = [Int]       
type ProgramCounter = Int 
type Program = [Command]  
type OutputLog = [Int]    

data MachineState = MachineState 
    { stack :: Stack
    , pc    :: ProgramCounter
    , program :: Program
    , outputs :: OutputLog
    } deriving Show

execute :: Command -> MachineState -> Either String MachineState
execute (Push x) state = 
    Right state { stack = x : stack state, pc = pc state + 1 }
execute Pop state = 
    case stack state of
        []     -> Left "Error: Pop on empty stack"
        (_:xs) -> Right state { stack = xs, pc = pc state + 1 }
execute Add state = 
    case stack state of
        (x:y:xs) -> Right state { stack = (y + x) : xs, pc = pc state + 1 }
        _        -> Left "Error: Not enough elements for Add"
execute Sub state = 
    case stack state of
        (x:y:xs) -> Right state { stack = (y - x) : xs, pc = pc state + 1 }
        _        -> Left "Error: Not enough elements for Sub"
execute Mul state = 
    case stack state of
        (x:y:xs) -> Right state { stack = (y * x) : xs, pc = pc state + 1 }
        _        -> Left "Error: Not enough elements for Mul"
execute Div state = 
    case stack state of
        (0:_)    -> Left "Error: Division by zero"
        (x:y:xs) -> Right state { stack = (y `div` x) : xs, pc = pc state + 1 }
        _        -> Left "Error: Not enough elements for Div"
execute (Jump pos) state = 
    Right state { pc = pos }
execute (JumpIfZero pos) state = 
    case stack state of
        (0:xs) -> Right state { stack = xs, pc = pos }
        (_:xs) -> Right state { stack = xs, pc = pc state + 1 }
        []     -> Left "Error: JumpIfZero on empty stack"
execute Output state = 
    case stack state of
        (x:xs) -> Right state { stack = xs, pc = pc state + 1, outputs = outputs state ++ [x] }
        []     -> Left "Error: Output on empty stack"

runProgram :: MachineState -> Either String MachineState
runProgram state
    | pc state >= length (program state) = Right state
    | otherwise = case execute (program state !! pc state) state of
        Left err -> Left err
        Right newState -> runProgram newState

parseCommand :: String -> Either String Command
parseCommand str = case words str of
    ["PUSH", n]       -> Right $ Push (read n)
    ["POP"]           -> Right Pop
    ["ADD"]           -> Right Add
    ["SUB"]           -> Right Sub
    ["MUL"]           -> Right Mul
    ["DIV"]           -> Right Div
    ["JUMP", pos]     -> Right $ Jump (read pos)
    ["JUMP_IF_ZERO", pos] -> Right $ JumpIfZero (read pos)
    ["OUTPUT"]        -> Right Output
    _                 -> Left $ "Unknown command: " ++ str

parseProgram :: String -> Either String Program
parseProgram input = sequence (map parseCommand (lines input))

data Expr
    = Const Int
    | AddExpr Expr Expr
    | SubExpr Expr Expr
    | MulExpr Expr Expr
    | DivExpr Expr Expr

exprToProgram :: Expr -> Program
exprToProgram (Const n) = [Push n]
exprToProgram (AddExpr x y) = exprToProgram x ++ exprToProgram y ++ [Add]
exprToProgram (SubExpr x y) = exprToProgram x ++ exprToProgram y ++ [Sub]
exprToProgram (MulExpr x y) = exprToProgram x ++ exprToProgram y ++ [Mul]
exprToProgram (DivExpr x y) = exprToProgram x ++ exprToProgram y ++ [Div]

exampleProgram :: Program
exampleProgram = [Push 10, Push 20, Add, Output]

main :: IO ()
main = do
    let initialState = MachineState { stack = [], pc = 0, program = exampleProgram, outputs = [] }
    case runProgram initialState of
        Left err -> putStrLn $ "Error: " ++ err
        Right finalState -> do
            putStrLn $ "Final stack: " ++ show (stack finalState)
            putStrLn $ "Output log: " ++ show (outputs finalState)