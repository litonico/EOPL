type Name = String

data LambdaExp = Var Name
               | MakeLambda Name LambdaExp
               | Apply LambdaExp LambdaExp

instance Show LambdaExp where
    show (Var name) = name
    show (MakeLambda name exp) =
        "(lambda (" ++ name ++ ") " ++ show exp ++ ")"
    show (Apply operator operand) =
        "(" ++ show operator ++ " " ++ show operand ++ ")"


type Value = String

data Lambda = Lambda { name :: Name, exp :: LambdaExp, env :: Env}

data Env = Empty
         | Extend Name LambdaExp Env

extendEnv :: Name -> LambdaExp -> Env -> Env
extendEnv var val env = Extend var val env

applyEnv :: Env -> Name -> Maybe LambdaExp
applyEnv Empty _ = Nothing
applyEnv (Extend name lambda env) searchName =
    if name == searchName
        then Just lambda
        else applyEnv env searchName

evalExp = Env -> Lambda

main :: IO ()
main = do
    putStrLn "Hello, Lambda"
