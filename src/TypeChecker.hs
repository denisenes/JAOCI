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
checkProgram x = case x of
  PDefs defs -> failure x

checkDef :: Def -> Result
checkDef x = case x of
  DefFun type_ id args stms -> failure x

checkArg :: Arg -> Result
checkArg x = case x of
  ArgDecl type_ id -> failure x

checkStm :: Stm -> Result
checkStm x = case x of
  StmExp exp -> failure x
  StmDecls type_ ids -> failure x
  StmInit type_ id exp -> failure x
  StmReturn exp -> failure x
  StmWhile exp stm -> failure x
  StmBlock stms -> failure x
  StmIfElse exp stm1 stm2 -> failure x

checkExp :: Exp -> Result
checkExp x = case x of
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

checkIncDecOp :: IncDecOp -> Result
checkIncDecOp x = case x of
  OpInc -> failure x
  OpDec -> failure x

checkMulOp :: MulOp -> Result
checkMulOp x = case x of
  OpTimes -> failure x
  OpDiv -> failure x

checkAddOp :: AddOp -> Result
checkAddOp x = case x of
  OpPlus -> failure x
  OpMinus -> failure x

checkCmpOp :: CmpOp -> Result
checkCmpOp x = case x of
  OpLt -> failure x
  OpGt -> failure x
  OpLtEq -> failure x
  OpGtEq -> failure x
  OpEq -> failure x
  OpNEq -> failure x

checkBool :: BoolLiteral -> Result
checkBool x = case x of
  LTrue -> failure x
  LFalse -> failure x

checkType :: Type -> Result
checkType x = case x of
  Type_bool -> failure x
  Type_int -> failure x
  Type_double -> failure x
  Type_void -> failure x

