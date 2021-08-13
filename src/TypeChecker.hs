module TypeChecker where

import Generated.ErrM
import AbsGrammar
import CheckUtils

--TODO: normal error messaging
--TODO: make type Result = Err () (i don't need any data in the positive Result case)

type Result = Err String

-- Result combinator
(<++>):: Result -> Result -> Result
(<++>) (Ok a) (Ok b) = Ok (a ++ "\n" ++ b)     --OOOOO PaTtErN mAtChInG 0_O
(<++>) (Bad a) (Bad b) = Bad (a ++ "\n" ++ b)
(<++>) (Bad a) (Ok b) = Bad a
(<++>) (Ok a) (Bad b) = Bad b

-- yes, i'm java developer ыыыы
throw :: Show a => a -> Result
throw x = Bad $ " * Undefined case: " ++ show x

checkProgram :: Program -> Result
checkProgram (PDefs defs) = let exenv = foldl (\x y -> extend x y) emptyEnv defs in --firstly add all function definions into environment
      foldl (checkDefs exenv) (Ok (show defs)) defs                                 --and then let's check all this definitions
            where 
            checkDefs::Env -> Result -> Def -> Result
            checkDefs env res def = let checkRes = checkDef env def in
                  if isBad $ checkRes then checkRes else res
            extend:: Env -> Def -> Env
            extend env (DefFun funType id args _) = let argTypes = map (\(ArgDecl t _) -> t) args in
                  updateFun env id (argTypes, funType)


checkDef:: Env -> Def -> Result
checkDef env (DefFun t id args stms) = let newenv = extend env args in --put all parameters into context
      (checkArgs args) <++> (checkStms (newenv, t) stms)
      where      
      extend:: Env -> [Arg] -> Env
      extend e [] = e
      extend e ((ArgDecl t id):argsTail) = let newenv = updateVar e id t in
            extend newenv argsTail 


-- args can't be void
checkArgs:: [Arg] -> Result
checkArgs args = foldl (\acc arg -> acc <++> (check arg)) (Ok (show args)) args
      where 
      check:: Arg -> Result
      check (ArgDecl type_ id) = case type_ of 
            Type_void -> Bad "The argument is void"
            _ -> Ok (show id ++ "checked")


checkStms:: (Env, Type) -> [Stm] -> Result
checkStms (e, _) [] = Ok (show e)
checkStms (e, t) (stm:tail) = let errEnv = checkStm (e, t) stm in
      case errEnv of
            (Ok newenv) -> checkStms (newenv, t) tail
            (Bad err) -> Bad err

checkStm:: (Env, Type) -> Stm -> Err Env --Type is used in return statements
checkStm (env, retype) s = case s of
      StmExp exp -> do
            inferExp env exp
            return env
      -- TODO are u crazy, what about checking if the variable was declared before mmmmmm?????????
      -- i'm such a dick!!!
      StmDecls type_ ids -> do
            checkDeclared env ids
            Ok (foldl (\e id -> updateVar e id type_) env ids) -- update environment
      StmInit type_ id exp -> do --TODO yeeee what about void type...
            checkDeclared env [id]
            checkExp env type_ exp                           -- check expression and then update environment
            let newenv = updateVar env id type_
            return newenv
      StmReturn exp -> do                                                -- check returning type
            checkExp env retype exp -- i'm not sure here, i'll test it   
            return env
      StmWhile exp stm -> do                                             -- just check exp and stm
            inferExp env exp
            checkStm (env, retype) stm
      StmBlock stms -> do
            let newenv = newBlock env
            checkStms (newenv, retype) stms                  -- push new table into the stack
            return env
      StmIfElse exp stm1 stm2 -> do                                      --simpl'e
            inferExp env exp
            checkStm (env, retype) stm1
            checkStm (env, retype) stm2
      where
      checkDeclared:: Env -> [Id] -> Result
      checkDeclared _ [] = Ok "declared"        
      checkDeclared env (id:tail) = case lookVar2 env id of
                                          Nothing -> checkDeclared env tail
                                          Just _ -> Bad (" * Variable " ++ (show id) ++ " was redeclared")

checkExp :: Env -> Type -> Exp -> Result
checkExp env type_ exp = do
      type2_ <- inferExp env exp
      if (type2_ == type_) then return (show env) else throw $ "type of " ++ (show exp)

inferExp:: Env -> Exp -> Err Type
inferExp env x = case x of
      ExpBool bool -> return Type_bool
      ExpInt integer -> return Type_int
      ExpDouble double -> return Type_double
      ExpId id -> inferId env id
      ExpApp id exps -> inferApp env id exps
      ExpPost id incdecop -> isNotBool $ (inferId env id)
      ExpPre incdecop id -> isNotBool $ (inferId env id)
      ExpMul exp1 mulop exp2 -> inferArithmBin env exp1 exp2
      ExpAdd exp1 addop exp2 -> inferArithmBin env exp1 exp2
      ExpCmp exp1 cmpop exp2 -> inferArithmBin env exp1 exp2
      ExpAnd exp1 exp2 -> inferArithmBin env exp1 exp2
      ExpOr exp1 exp2 -> inferArithmBin env exp1 exp2
      ExpAssg id exp -> inferAssg env id exp
      where
      isNotBool:: Err Type -> Err Type
      isNotBool type_ = case type_ of
                              Bad str -> Bad str
                              Ok Type_bool -> Bad " * Unresolved operation in boolean expression"
                              Ok t -> return t 

inferAssg:: Env -> Id -> Exp -> Err Type
inferAssg env id exp = do
      let id_type = lookVar env id
      case id_type of
            Nothing -> Bad (" * Variable " ++ (show id) ++ " is not declared in this scope")
            (Just type_) -> do
                  let res = checkExp env type_ exp
                  case res of
                        (Bad s) -> do
                              exp_type <- inferExp env exp
                              Bad (" * Assignment error, could't match " ++ (show type_) ++ " with " ++ (show exp_type))
                        (Ok _) ->  return type_


inferId:: Env -> Id -> Err Type
inferId env id = do
      let res = lookVar env id
      case res of
            Nothing -> Bad (" * Variable " ++ (show id) ++ " is not declared in this scope")
            Just type_ -> return type_

inferArithmBin :: Env -> Exp -> Exp -> Err Type
inferArithmBin env a b = do
      type_ <- inferExp env a
      if elem type_ [Type_int, Type_double] then do
            let res = checkExp env type_ b
            case res of
                  (Bad s) -> (Bad s)
                  (Ok _) -> return type_
      else Bad (" * Type of expression " ++ (show b))

inferApp:: Env -> Id -> [Exp] -> Err Type
inferApp env id exps = do
      let maybe_fun = lookFun env id --who knows...
      case maybe_fun of
            Nothing -> Bad (" * Function " ++ (show id) ++ " is not declared")
            Just (arg_types, ret_type) -> do
                  checkArgTypes env arg_types exps
                  return ret_type
      where
      checkArgTypes:: Env -> [Type] -> [Exp] -> Result
      checkArgTypes env [] [] = Ok (show env)                                   -- we checked all args, life is beautiful
      checkArgTypes _ types [] = Bad " * Not enough arguments in funtion call"  -- not enough expressions
      checkArgTypes _ [] exps = Bad " * Too many arguments in function call"    -- too many expressions
      checkArgTypes env (type_:types) (exp:exps) = if isOk $ checkExp env type_ exp then
            checkArgTypes env types exps
            else Bad (" * Argument " ++ (show exp) ++ " is not valid")