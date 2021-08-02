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
throw x = Bad $ "Undefined case: " ++ show x

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
      StmDecls type_ ids -> Ok (foldl (\e id -> updateVar e id type_) env ids) -- update environment
      StmInit type_ id exp -> do --TODO yeeee what about void type...
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

checkExp :: Env -> Type -> Exp -> Result
checkExp env type_ exp = do
      type2_ <- inferExp env exp
      if (type2_ == type_) then return (show env) else throw $ "type of " ++ (show exp)

inferExp:: Env -> Exp -> Err Type
inferExp env x = case x of
      ExpBool bool -> Bad "dummy exp"
      ExpInt integer -> Bad "dummy exp"
      ExpDouble double -> Bad "dummy exp"
      ExpId id -> Bad "dummy exp"
      ExpApp id exps -> Bad "dummy exp"
      ExpPost id incdecop -> Bad "dummy exp"
      ExpPre incdecop id -> Bad "dummy exp"
      ExpMul exp1 mulop exp2 -> Bad "dummy exp"
      ExpAdd exp1 addop exp2 -> Bad "dummy exp"
      ExpCmp exp1 cmpop exp2 -> Bad "dummy exp"
      ExpAnd exp1 exp2 -> Bad "dummy exp"
      ExpOr exp1 exp2 -> Bad "dummy exp"
      ExpAssg id exp -> Bad "dummy exp"

