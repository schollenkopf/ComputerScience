
module printmodule
let rec printC c =
    match c with
        |INIT(a,b) ->  "INIT ("+ printE(a)+" , "+printE(b) + ")" 
        |TWOcommand(a,b) ->  "TWOcommands ("+printC(a)+" ; "+printC(b) + ")"
        |IFFI(a)         ->  "IFFI ("+printG(a)+")"
        |DOOD(a)         ->   "DOOD ("+printG(a)+")"
        |Skip            ->   "SKIP "

and printG g =
    match g with        
        |Follows(a,b)   -> "FOLLOWS ("+printB(a)+" -> " + printC(b)+")"
        |TWOguardedC(a,b) -> "TWOguardedC ("+printG(a)+" [] "+printG(b)+")"



and printB b =
    match b with
        |Boolean(a) -> if a then "true" else "false"
        |And(a,b)   -> "("+printB(a)+" & "+printB(b)+")"
        |Andand(a,b)   -> "("+printB(a)+" && "+printB(b)+")"
        |Or(a,b)   -> "("+printB(a)+" | "+printB(b)+")"
        |Oror(a,b)   -> "("+printB(a)+" || "+printB(b)+")"
        |Equal(a,b)  -> "("+printE(a)+" = "+printE(b)+")"
        |NOTEqual(a,b)  -> "("+printE(a)+" != "+printE(b)+")"
        |Not(a)         -> "(!"+printB(a)+")"
        |GREATER(a,b)  -> "("+printE(a)+" > "+printE(b)+")"
        |LESS(a,b)  -> "("+printE(a)+" < "+printE(b)+")"
        |GREATEREqual(a,b)  -> "("+printE(a)+" >= "+printE(b)+")"
        |LESSEqual(a,b)  -> "("+printE(a)+" <= "+printE(b)+")"

and printE e =
    match e with
         | Num(a)  -> string a
         | Name(a)    -> a
         | NAMEArray(s,i)   -> s+"["+printE(i)+"]"
         | TimesExpr(a,b)   -> "("+printE(a)+" * "+printE(b)+")"
         | DivExpr(a,b)     -> "("+printE(a)+" / "+printE(b)+")"
         | PlusExpr(a,b)     -> "("+printE(a)+" + "+printE(b)+")"
         | MinusExpr(a,b)   -> "("+printE(a)+" - "+printE(b)+")"
         | PowExpr(a,b)     -> "("+printE(a)+" ^ "+printE(b)+")"
         | UPlusExpr(a)  -> "+ ("+printE(a)+")"
         | UMinusExpr(a)  ->  "- ("+printE(a)+")"
