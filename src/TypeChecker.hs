module TypeChecker where

import Generated.ErrM
import AbsGrammar
import CheckUtils

type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

checkId :: Id -> Result
checkId x = case x of
      Id string -> failure x

checkProgram :: Program -> Result
checkProgram (PDefs defs) = let exenv = foldl (\x y -> extend x y) emptyEnv defs in --firstly add all function definions into environment
      foldl (checkDefs exenv) (Ok (show defs)) defs where --and then let's check all this definitions
            checkDefs::Env -> Result -> Def -> Result
            checkDefs env res def = let checkRes = checkDef env def in
                  if isBad $ checkRes then checkRes else res
            extend:: Env -> Def -> Env
            extend env (DefFun funType id args _) = let argTypes = map (\(ArgDecl t _) -> t) args in
                  updateFun env id (argTypes, funType)


checkDef:: Env -> Def -> Result
checkDef env (DefFun t id args stms) = 

-- I'M HERE --

checkArg:: Env -> Arg -> Result
checkArg env x = case x of
      ArgDecl type_ id -> failure x

checkStm:: Env -> Stm -> Result
checkStm env x = case x of
      StmExp exp -> failure x
      StmDecls type_ ids -> failure x
      StmInit type_ id exp -> failure x
      StmReturn exp -> failure x
      StmWhile exp stm -> failure x
      StmBlock stms -> failure x
      StmIfElse exp stm1 stm2 -> failure x

checkExp:: Env -> Exp -> Result
checkExp env x = case x of
      ExpBool bool -> failure x
      ExpInt integer -> failure x
      ExpDouble double -> failure x
      ExpId id -> failure x
      ExpApp id exps -> failure x
      ExpPost id incdecop -> failure x
      ExpPre incdecop id -> failure x
      ExpMul exp1 mulop exp2 -> failure x
      ExpAdd exp1 addop exp2 -> failure x
      ExpCmp exp1 cmpop exp2 -> failure x
      ExpAnd exp1 exp2 -> failure x
      ExpOr exp1 exp2 -> failure x
      ExpAssg id exp -> failure x

checkIncDecOp:: Env -> IncDecOp -> Result
checkIncDecOp env x = case x of
      OpInc -> failure x
      OpDec -> failure x

checkMulOp:: Env -> MulOp -> Result
checkMulOp env x = case x of
      OpTimes -> failure x
      OpDiv -> failure x

checkAddOp:: Env -> AddOp -> Result
checkAddOp env x = case x of
      OpPlus -> failure x
      OpMinus -> failure x

checkCmpOp:: Env -> CmpOp -> Result
checkCmpOp env x = case x of
      OpLt -> failure x
      OpGt -> failure x
      OpLtEq -> failure x
      OpGtEq -> failure x
      OpEq -> failure x
      OpNEq -> failure x

checkBool:: Env -> BoolLiteral -> Result
checkBool env x = case x of
      LTrue -> failure x
      LFalse -> failure x

checkType:: Env -> Type -> Result
checkType env x = case x of
      Type_bool -> failure x
      Type_int -> failure x
      Type_double -> failure x
      Type_void -> failure x

