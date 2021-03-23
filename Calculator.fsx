// This script implements our interactive calculator




// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System
#load "CalculatorTypesAST.fs"
open CalculatorTypesAST
#load "CalculatorParser.fs"
open CalculatorParser
#load "CalculatorLexer.fs"
open CalculatorLexer
#load "print.fs"
#load "Compiler.fs"
open Compiler
#load "CompilerD.fs"
#load "Interpreter.fs"
open Interpreter
open CompilerD



// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
// let rec eval e =
//   match e with
//     | Num(x) -> x
//     | TimesExpr(x,y) -> eval(x) * eval (y)
//     | DivExpr(x,y) -> eval(x) / eval (y)
//     | PlusExpr(x,y) -> eval(x) + eval (y)
//     | MinusExpr(x,y) -> eval(x) - eval (y)
//     | PowExpr(x,y) -> eval(x) ** eval (y)
//     | UPlusExpr(x) -> eval(x)
//     | UMinusExpr(x) -> - eval(x)
//     | Name(x)  -> x
//     | NAMEArray(a,b) -> a[b]
    
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
        |And(a,b)   -> "AND ("+printB(a)+" and "+printB(b)+")"
        |Andand(a,b)   -> "ANDAND ("+printB(a)+" andand "+printB(b)+")"
        |Or(a,b)   -> "OR ("+printB(a)+" or "+printB(b)+")"
        |Oror(a,b)   -> "OROR ("+printB(a)+" oror "+printB(b)+")"
        |Equal(a,b)  -> "EQUAL ("+printE(a)+" = "+printE(b)+")"
        |NOTEqual(a,b)  -> "NOTEQUAL ("+printE(a)+" , "+printE(b)+")"
        |Not(a)         -> "NOT ("+printB(a)+")"
        |GREATER(a,b)  -> "GREATER ("+printE(a)+" , "+printE(b)+")"
        |LESS(a,b)  -> "LESS ("+printE(a)+" , "+printE(b)+")"
        |GREATEREqual(a,b)  -> "GREATEREQUAL ("+printE(a)+" , "+printE(b)+")"
        |LESSEqual(a,b)  -> "LESSEQUAL ("+printE(a)+" , "+printE(b)+")"

and printE e =
    match e with
         | Num(a)  -> string a
         | Name(a)    -> a
         | NAMEArray(s,i)   -> s+"["+printE(i)+"]"
         | TimesExpr(a,b)   -> "TIMES ("+printE(a)+" , "+printE(b)+")"
         | DivExpr(a,b)     -> "DIV ("+printE(a)+" , "+printE(b)+")"
         | PlusExpr(a,b)     -> "PLUS ("+printE(a)+" , "+printE(b)+")"
         | MinusExpr(a,b)   -> "MINUS ("+printE(a)+" , "+printE(b)+")"
         | PowExpr(a,b)     -> "POWER ("+printE(a)+" , "+printE(b)+")"
         | UPlusExpr(a)  -> "+"+printE(a)
         | UMinusExpr(a)  ->  "-"+printE(a)



// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = CalculatorParser.start CalculatorLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

let rec getFirst n = 
    match n with
    |[] -> []
    |(a,b)::xs -> a::getFirst xs

let rec toMem d map =
    match d with
    |TWOcommand(a,b) -> toMem a (toMem b map)
    |INIT(NAMEArray(a,c),b) -> let st = a + "["+(string(get_int(eval_expr c map)))+"]"
                               Map.add (st) (get_int(eval_expr b map)) map
    |INIT(Name(a),b) -> Map.add a (get_int(eval_expr b map)) map 
    |_ -> failwith "invalid initialization"

// We implement here the function that interacts with the user
let rec compute n =
    printfn "You have %i tries left"(n)
    if n = 0 then

        printfn "Bye bye"
    
    else
       
        for arg in fsi.CommandLineArgs do
            if arg="ND" then
                printfn("Non-Deterministic MODE")
                printf "Enter guarded command code: "
                try
                  let e = parse (Console.ReadLine())
                  printfn "Result: %s" (toDot  (edgesC e (Node("qstart")) (Node("qend")) 1)  "digraph program_graph {rankdir=LR;node [shape = circle]; qstart; node [shape = doublecircle]; qend; node [shape = circle]")                                                            
                  compute n
                with err -> compute (n-1)
            else if arg="D" then
                     printfn("Deterministic MODE")
                     printf "Enter guarded command code: "
                     try
                         let e = parse (Console.ReadLine())
                         printfn "Result: %s" (toDotD  (edgesCD e (Node("qstart")) (Node("qend")) 1 (Boolean(false)))  "digraph program_graph {rankdir=LR;node [shape = circle]; qstart; node [shape = doublecircle]; qend; node [shape = circle]")                                                          
                         compute n
                     with err -> compute (n-1)
                 else if arg="Calculator.fsx" then printfn("\n")
                      else if arg="Interpreter" then
                              printfn("Interpreting")
                              
                              try
                                 printf "Initialize variables: "
                                 let init = parse (Console.ReadLine())
                                 printf "Enter guarded command code: "
                                 let e = parse (Console.ReadLine())
                                 let p = getFirst (edgesCD e (Node("qstart")) (Node("qend")) 1 (Boolean(false)))
                                 let r = step_exe (p) (p) (toMem init Map.empty) "qstart" 0
                                 printfn  "%s" (fst r)
                                
                                 printfn  "%A" (snd r)
                              with err -> compute (n-1)
                           else printfn("no")
                            
                           

//add map input


// Start interacting with the user
compute 10
