module Compiler
open printmodule

let factory  = 
    let counter = ref 1
    fun a -> if a then counter.Value <- !counter + 1
                       !counter
             else counter.Value <- !counter
                  !counter
let rec edgesC c s e n =
    match c with
        |INIT(a,b) ->  [Edge(s,Assignment(a,b),e)]
        |TWOcommand(a,b) ->  edgesC a  s (Node(sprintf "q%i" (n)))  (factory(true)) @ edgesC b (Node(sprintf "q%i" (n))) e (factory(false))    
        |IFFI(a)         ->  edgesG a s e (factory(false))
        |DOOD(a)         ->  edgesG a s s (factory(false)) @ [Edge(s,Baction(Done a), e)]
        |Skip            ->   [Edge(s,Saction,e)]

        
    
and Done a = 
    match a with 
    |Follows(a,b)   -> Not a
    |TWOguardedC(a,b) -> And (Done a,Done b)

and edgesG g s e n =
    match g with        
        |Follows(a,b)   ->  [Edge(s, Baction(a) , (Node(sprintf "q%i" (factory(false)))))] @ edgesC b (Node(sprintf "q%i" (factory(false)))) e (factory(true))
        |TWOguardedC(a,b) -> edgesG a s e  (factory(false)) @ edgesG b s e (factory(false))
        

let getNode b = 
    match b with
    |Node(a) -> a

let getLabel d= 
    match d with
    | Assignment(a,b) -> printE(a) + " := " + printE(b)
    | Saction         -> "Skip"
    | Baction(b)         ->  printB(b)


let rec toDot e dot=
    match e with
    | [] -> dot + "}"
    | Edge(a,b,c)::xs -> toDot xs (dot + (getNode(a)) + "->" + (getNode(c)) + "[label = \"" + (getLabel(b)) + "\"];\n")
    

