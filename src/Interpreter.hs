module Interpreter where

import Generated.ErrM
import AbsGrammar
import InterpUtils

-- evaluate expression and return value and new environment
eval:: IEnv -> Exp -> (Value, IEnv)
eval env exp = case exp of
    ExpBool LTrue -> (Bool True, env)
    ExpBool LFalse -> (Bool False, env)
    ExpInt integer -> (Int integer, env)
    ExpDouble double -> (Double double, env)
    ExpId id -> let (Just val) = (lookIVar env id) in (val, env)
    ExpApp id exps -> (call env id exps, env)
    ExpPost id incdecop -> (post_val, post_env)
        where
        post_val = let (Just value) = lookIVar env id in value
        post_env = case incdecop of
            OpInc -> extendVar env id $ post_val + 1
            OpDec -> extendVar env id $ post_val - 1
    ExpPre incdecop id -> (pre_val, pre_env)
        where
        pre_val = let (Just value) = lookIVar env id in
            case incdecop of
            OpInc -> value + 1
            OpDec -> value - 1
        pre_env = extendVar env id pre_val
    ExpMul exp1 mulop exp2 -> case mulop of
            OpTimes -> (val1 * val2, env2)
            OpDiv -> (val1 `vdiv` val2, env2)
        where
        (val1, env1) = eval env exp1
        (val2, env2) = eval env1 exp2
    ExpAdd exp1 addop exp2 -> case addop of 
            OpPlus ->  (val1 + val2, env2)
            OpMinus -> (val1 - val2, env2)
        where
        (val1, env1) = eval env exp1
        (val2, env2) = eval env1 exp2
    ExpCmp exp1 cmpop exp2 -> case cmpop of
            OpLt   -> (Bool $ val1 < val2, env2)
            OpGt   -> (Bool $ val1 > val2, env2)
            OpLtEq -> (Bool $ val1 <= val2, env2)
            OpGtEq -> (Bool $ val1 >= val2, env2)
            OpEq   -> (Bool $ val1 == val2, env2)
            OpNEq  -> (Bool $ val1 /= val2, env2)
        where
        (val1, env1) = eval env exp1
        (val2, env2) = eval env1 exp2
    ExpAnd exp1 exp2 -> (val1 `vand` val2, env2)
        where
        (val1, env1) = eval env exp1
        (val2, env2) = eval env1 exp2
    ExpOr exp1 exp2 ->  (val1 `vor` val2, env2)
        where
        (val1, env1) = eval env exp1
        (val2, env2) = eval env1 exp2
    ExpAssg id exp -> let (val, env1) = eval env exp in
        (val, extendVar env1 id val)

call:: IEnv -> Id -> [Exp] -> Value
call env id exps = ret_result
    where
    newenv = callPush env1 args vals
    ret_result = execStms newenv stms
    (Just (args, stms)) = lookIFun env id
    (vals, env1) = foldl (\x y -> lamda x y) ([], env) exps
    lamda (acc, penv) exp = let (val, nenv) = eval penv exp in ((val:acc), nenv)

--Env exec (Env env, [Stm] s)
execStms:: IEnv -> [Stm] -> Value
execStms env [] = Void
execStms env (stm:tail) = case stm of
        StmReturn exp -> let (val, _) = eval env exp in val
        _ -> execStms newenv tail
        where newenv = exec env stm
        

--Env exec (Env env, Stm s)
exec:: IEnv -> Stm -> IEnv
exec env stm = env


--Void exec (Program p)
execProg:: Program -> Value -- returns exit code of main
execProg (PDefs prog) = let env = makeEnv emptyIEnv prog in
    call env (Id "main") []
    where
    makeEnv:: IEnv -> [Def] -> IEnv  -- extends environment with function definitions
    makeEnv env [] = env
    makeEnv env (def:tail) = let newenv = extendFun env def in
        makeEnv newenv tail