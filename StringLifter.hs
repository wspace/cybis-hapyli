module StringLifter (liftStringsInProgram) where
import Ast


liftStringsInProgram :: Ast.Program -> Ast.Program
liftStringsInProgram (Program variables callables) =
    let (id, liftedVars, newCallables) = applyLift liftFromCallable 0 callables
    in  Ast.Program (liftedVars++variables) newCallables

    
applyLift :: (Int -> a -> (Int, [Variable], a)) -> 
             Int ->
             [a] ->
             (Int, [Variable], [a])
    
applyLift f id = foldr combine (id, [], [])
    where combine x (id, vars, newXs) =
            let (nextId, newVars, newX) = f id x
            in  (nextId, newVars++vars, newX:newXs)

          
liftFromCallable :: Int -> Callable -> (Int, [Variable], Callable)
liftFromCallable id m@(Macro _ _ _) = (id, [], m)

liftFromCallable id (Function name params bindings body) =
    let (letSymbols, letExpressions) = unzip bindings
        (nextId, newVars, newBody:newLetExprs) = 
          applyLift liftFromExpression id (body:letExpressions)
        newBindings = zip letSymbols newLetExprs
        newFunction = Function name params newBindings newBody
    in  (nextId, newVars, newFunction)
    

liftFromExpression :: Int -> Expression -> (Int, [Variable], Expression)
liftFromExpression id (IntegerLiteral i) = (id, [], IntegerLiteral i)
liftFromExpression id (Symbol name) = (id, [], Symbol name)

liftFromExpression id (StringLiteral str) =
    let name = "~str" ++ (show id)
    in  (id + 1, [(StringVariable name str)], Symbol name)

liftFromExpression id (CallEx name args) =
    let (nextId, newVars, newArgs) = 
          applyLift liftFromExpression id args
    in  (nextId, newVars, CallEx name newArgs)

liftFromExpression id (If c t f) =
    let (nextId, newVars, nc:nt:nf:[]) = 
          applyLift liftFromExpression id [c, t, f]
    in  (nextId, newVars, If nc nt nf)
    
liftFromExpression id (Do exprs) =
    let (nextId, newVars, newExprs) = 
          applyLift liftFromExpression id exprs
    in  (nextId, newVars, Do newExprs)
    
 