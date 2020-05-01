module Expression

open FStar.Int

type error = | Error: error

type operation =
    | Leq : operation
    | Eq : operation
    | Geq : operation
    | IntValue: operation

type lambda = 
    | Operation: operation -> lambda
    | Invalid: lambda

// N.B. we map A -> B to tuple A * B (for some reason equality on function types does not work with my temper here...)
let evalType (l:lambda) = match l with
    | Operation Leq -> (int * int)
    | Operation IntValue -> int
    | _ -> error

// N.B. the noeq decorator
noeq type expression a =
    | Value: (l: lambda { (evalType l) == a }) -> a -> expression a
    | Apply1: (#b: Type) -> (l: lambda { (evalType l) == (b * a) }) -> expression b -> expression a


