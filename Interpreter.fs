module Interpreter

type interpreter_bool = 
    |Intertrue
    |Interfalse 
    |Undefined 

type interpreter_expr =
    |Number of int 
    |UndefinedExpr 

let get_int n =
    match n with
    |Number(i) -> i 
    |_ -> 0
 



let rec eval_expr expr mem = 
    match expr with 
    | Num(a)  -> Number (a)
    | Name(a)    -> if Map.containsKey a mem then Number (Map.find a mem)  else UndefinedExpr
    | NAMEArray(s,i)   -> let st = s + "["+(string(get_int(eval_expr i mem)))+"]"
                          if Map.containsKey (st) mem then Number (Map.find (st ) mem)  else UndefinedExpr
    | TimesExpr(a,b)   -> match (eval_expr a mem , eval_expr b mem) with
                          | (Number (a1), Number(b1)) -> Number (a1 * b1) 
                          | _ -> UndefinedExpr
    | DivExpr(a,b)     -> match (eval_expr a mem , eval_expr b mem) with
                            | (Number (a1), Number(b1)) -> Number (a1 / b1) 
                            | _ -> UndefinedExpr
    | PlusExpr(a,b)     -> match (eval_expr a mem , eval_expr b mem) with
                           | (Number (a1), Number(b1)) -> Number (a1 + b1) 
                           | _ -> UndefinedExpr
    | MinusExpr(a,b)   -> match (eval_expr a mem , eval_expr b mem) with
                          | (Number (a1), Number(b1)) -> Number (a1 - b1) 
                          | _ -> UndefinedExpr
    | PowExpr(a,b)     -> match (eval_expr a mem , eval_expr b mem) with
                          | (Number (a1), Number(b1)) -> Number (int(float(a1) ** float(b1))) 
                          | _ -> UndefinedExpr
    | UPlusExpr(a)  -> match eval_expr a mem with
                       | Number (a1) -> Number (a1) 
                       | _ -> UndefinedExpr
    | UMinusExpr(a)  ->  match eval_expr a mem with
                         | Number (a1) -> Number (- a1) 
                         | _ -> UndefinedExpr

let rec eval_bool c mem =
    match c with
        |Boolean(a) -> if a then Intertrue else Interfalse
        |And(a,b)   ->  match (eval_bool a mem , eval_bool b mem) with 
                        | (Intertrue, Intertrue) -> Intertrue 
                        | (Intertrue,Interfalse) -> Interfalse
                        | (Interfalse,Intertrue) -> Interfalse
                        | (Interfalse,Undefined) -> Interfalse
                        | (_,Undefined) -> Undefined
                        | (Undefined,_) -> Undefined
                        | (Interfalse,Interfalse) -> Interfalse

        |Andand(a,b)   -> match (eval_bool a mem , eval_bool b mem) with 
                            | (Intertrue, Intertrue) -> Intertrue 
                            | (Intertrue,Interfalse) -> Interfalse
                            | (Interfalse,Intertrue) -> Interfalse
                            | (_,Undefined) -> Undefined
                            | (Undefined,_) -> Undefined
                            | (Interfalse,Interfalse) -> Interfalse
        |Or(a,b)   -> match (eval_bool a mem , eval_bool b mem) with 
                        | (Intertrue, Intertrue) -> Intertrue 
                        | (Intertrue,Interfalse) -> Intertrue
                        | (Interfalse,Intertrue) -> Intertrue
                        | (Intertrue,Undefined) -> Intertrue
                        | (_,Undefined) -> Undefined
                        | (Undefined,_) -> Undefined
                        | (Interfalse,Interfalse) -> Interfalse
        |Oror(a,b)   -> match (eval_bool a mem , eval_bool b mem) with 
                            | (Intertrue, Intertrue) -> Intertrue 
                            | (Intertrue,Interfalse) -> Intertrue
                            | (Interfalse,Intertrue) -> Intertrue
                            | (_,Undefined) -> Undefined
                            | (Undefined,_) -> Undefined
                            | (Interfalse,Interfalse) -> Interfalse
        |Equal(a,b)  -> match (eval_expr a mem,eval_expr b mem) with 
                        |(Number (a1), Number (b1)) -> if a1=b1 then Intertrue else Interfalse
                        |_ -> Undefined
        |NOTEqual(a,b)  -> match (eval_expr a mem,eval_expr b mem) with 
                           |(Number (a1), Number (b1)) -> if (a1<>b1) then Intertrue else Interfalse
                           |_ -> Undefined
        |Not(a)         -> match eval_bool a mem with 
                           | Intertrue -> Interfalse
                           | Interfalse -> Intertrue
                           |_ -> Undefined
        |GREATER(a,b)  -> match (eval_expr a mem,eval_expr b mem) with 
                          |(Number (a1), Number (b1)) -> if a1>b1 then Intertrue else Interfalse
                          |_ -> Undefined
        |LESS(a,b)  -> match (eval_expr a mem,eval_expr b mem) with 
                       |(Number (a1), Number (b1)) -> if a1<b1 then Intertrue else Interfalse
                       |_ -> Undefined
        |GREATEREqual(a,b)  -> match (eval_expr a mem,eval_expr b mem) with 
                               |(Number (a1), Number (b1)) -> if a1>=b1 then Intertrue else Interfalse
                               |_ -> Undefined
        |LESSEqual(a,b)  -> match (eval_expr a mem,eval_expr b mem) with 
                            |(Number (a1), Number (b1)) -> if a1<=b1 then Intertrue else Interfalse
                            |_ -> Undefined

let defined action mem = 
    match action with 
    |Assignment(NAMEArray(a,c),b) -> let st = a + "["+(string(get_int(eval_expr c mem)))+"]"
                                     match ((Map.containsKey st mem) , (eval_expr b mem )) with 
                                     |(true, Number (a1)) -> Intertrue
                                     |_ -> Undefined 
    |Assignment(Name(a),b) -> match ((Map.containsKey a mem) , (eval_expr b mem )) with 
                              |(true, Number (a1)) -> Intertrue
                              |_ -> Undefined 
    |Saction         -> Intertrue
    |Baction(b)         ->  eval_bool b mem
    |_ -> failwith "this action doesnt exist"

let rec change_memory action mem = 
    match action with 
    |Assignment(NAMEArray(a,c),b) -> let st = a + "["+(string(get_int(eval_expr c mem)))+"]"
                                     match eval_expr b mem with
                                      | Number(a1) -> Map.add st a1 mem
                                      |UndefinedExpr -> mem
    |Assignment(Name (a),b) -> match eval_expr b mem with
                                 | Number(a1) -> Map.add a a1 mem
                                 |UndefinedExpr -> mem
    |Saction         -> mem
    |Baction(b)         -> mem
    |_ -> failwith "this action doesnt exist"

let rec step_exe all_edges edges mem state n= 
    match edges with
    |[] -> if state = "qend" then ("Finished execution",mem) else ("Interpreter stuck in node " + state, mem)  
    |Edge(Node(q1),a,Node(q2))::xs when q1=state && ((defined a mem) = Intertrue) ->  printfn "Cycle: %i" (n)
                                                                                      printfn "%s" (state) 
                                                                                      printfn "%A" (mem) 
                                                                                      printfn "\n"
                                                                                      step_exe all_edges all_edges (change_memory a mem) q2  (n+1)
    |_ :: tail -> step_exe all_edges tail mem state n



let rec step_exeND all_edges edges mem state n options=
    match edges with
    |[] when ((List.length options) = 0) -> if state = "qend" then ("Finished execution",mem) else ("Interpreter stuck in node " + state, mem) 
    |[] ->   let o = new Random()
             let r = o.Next(0,(List.length options))
             let randomoption = options.Item(r)
             printfn "Cycle: %i" (n)
             printfn "%s" (state) 
             printfn "%A" (mem) 
             printfn "\n"
             step_exeND all_edges all_edges (change_memory (fst randomoption) mem) (snd randomoption)  (n+1) ([])
    |Edge(Node(q1),a,Node(q2))::xs when q1=state && ((defined a mem) = Intertrue) ->  step_exeND all_edges xs mem state n ((a,q2)::options)
                                                                                      
    |_ :: tail -> step_exeND all_edges tail mem state n options

