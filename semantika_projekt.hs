import Control.Monad.State

data Term = 
    Var Int 
  | Lam String Ty Term  -- Lam constructor now has parameter name, type and implementation
  | App Term Term
  deriving (Show, Eq)

data Ty =
    TyBool
  | TyInt
  | TyArr Ty Ty -- function that takes Ty and returns Ty
  deriving (Show, Eq)

-- VarBind constructor used for assigning types to variables 
data Binding = NameBind | VarBind Ty
  deriving (Show, Eq)

-- list of variableName, binding, keeps track of type for every single variable
type Context = [(String, Binding)] 

-- used for modifying existing context, introduced for next reason: i had to predefine context with variables so it would work, now i just pass empty context
-- and it is automatically generated
type TypeCheckState = State Context

-- addBinding function takes variableName and binding: adds new variable to EXISTING context 
addBinding :: String -> Binding -> TypeCheckState ()
addBinding x bind = modify ((x, bind) :)


-- Function to get the binding for a given variable index
getBinding :: Int -> TypeCheckState (Either String Binding)
getBinding i = do
  ctx <- get  -- get current state
  if i < length ctx
    then return $ Right (snd (ctx !! i))
    else return $ Left "Context list index out of bounds"


-- Function to get the type from the context for a given variable index
getTypeFromContext :: Int -> TypeCheckState (Either String Ty)
getTypeFromContext i = do
  result <- getBinding i
  case result of
    Left err -> return $ Left err
    Right binding -> case binding of
      NameBind -> return $ Left $ "getTypeFromContext: Wrong kind of binding for variable " ++ show binding
      VarBind tyT -> return $ Right tyT


typeof :: Term -> TypeCheckState (Either String Ty)
typeof t = case t of
  Var i -> getTypeFromContext i     -- If term is a variable, get its type from context

  -- If term is a function:
  Lam x argumentType implementation -> do
    addBinding x (VarBind argumentType)     -- add the variable to context with its type
    result <- typeof implementation     -- type-check the return type of the function
    case result of
      Right implementationType -> return $ Right (TyArr argumentType implementationType)    -- if return type checks, return function type
      Left err -> return $ Left err     -- catch any type-checking errors

-- If term is application:
  App t1 t2 -> do
    result1 <- typeof t1    -- type-check the function part of the application
    result2 <- typeof t2    -- type-check the argument part of the application
    
    case (result1, result2) of
      (Right (TyArr tyT11 tyT12), Right tyT2) ->    -- function type has to be TyArr
        if tyT2 == tyT11
          then return $ Right tyT12  -- if argument type matches expected parameter type, return function return type
          else return $ Left "parameter type mismatch"  -- otherwise, mismatch error
      (Right _, Left err) -> return $ Left err  -- catch type errors from argument type-checking
      (Left err, _) -> return $ Left err  -- catch type errors from function type-checking
      _ -> return $ Left "arrow type expected"  -- if neither case matches, arrow type was expected


-- only for testing purposes: tests typeof function
runTypeChecker :: Term -> Either String Ty
runTypeChecker term = evalState (typeof term) []


shift :: Int -> Term -> Term
shift d t = f 0 t
  where 
    f c (Var x) = if x >= c then Var (d + x) else Var x
    f c (Lam x tyT t) = Lam x tyT (f (c + 1) t)
    f c (App t1 t2) = App (f c t1) (f c t2)


subst :: Int -> Term -> Term -> Term
subst j s t = f 0 t
  where 
    f c (Var x) = if x == j + c then shift c s else Var x
    f c (Lam x tyT t) = Lam x tyT (f (c + 1) t)
    f c (App t1 t2) = App (f c t1) (f c t2)


beta :: Term -> Term -> Term
beta s t = shift (-1) $ subst 0 (shift 1 s) t

eval_step :: Term -> Term
eval_step (Var i) = Var i

eval_step (Lam x tyT1 t2) = Lam x tyT1 (eval_step t2)

eval_step (App t1 t2) =
  case eval_step t1 of
    Lam x _ t -> eval_step (beta t2 t) 
    t1' -> App t1' (eval_step t2)


eval :: Term -> Term
eval t = if t == t' then t else eval t'
  where t' = eval_step t


runTypeCheckAndEval :: Term -> Either String (Term, Ty)
runTypeCheckAndEval term =
  case evalState (typeof term) [] of
    Right ty -> Right (eval term, ty) -- if type check passes then evaluate term and return reduced type
    Left err -> Left err


-- Identity function (\x -> x)
term1 :: Term
term1 = Lam "x" TyBool (Var 0)
-- Expected type: TyArr TyBool TyBool

-- Application of identity function
term2 :: Term
term2 = App term1 (Var 0)
-- Expected type: TyBool 

-- Function that takes 2 variables and returns first
term3 :: Term
term3 = Lam "x" TyInt (Lam "y" TyBool (Var 1))
-- Expected type: TyArr TyInt (TyArr TyBool TyInt)

-- Function composition
term4 :: Term
term4 = Lam "f" (TyArr TyInt TyInt) (Lam "g" (TyArr TyInt TyInt) (Lam "x" TyInt (App (Var 2) (App (Var 1) (Var 0)))))
-- Expected type: TyArr (TyArr TyInt TyInt) (TyArr (TyArr TyInt TyInt) (TyArr TyInt TyInt))


main :: IO ()
main = do
  let terms = [term1, term2, term3, term4]
      results = map runTypeCheckAndEval terms
  mapM_ printResult (zip [1..] results)

printResult :: (Int, Either String (Term, Ty)) -> IO ()
printResult (n, result) = do
  putStrLn $ "Term" ++ show n ++ ":"
  case result of
    Right (term, ty) -> do
      putStrLn $ "  Type: " ++ show ty
      putStrLn $ "  Evaluated: " ++ show term
    Left err -> putStrLn $ "  Error: " ++ err