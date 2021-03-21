module CompilerD
open printmodule

let factory  = 
    let counter = ref 1
    fun a -> if a then counter.Value <- !counter + 1
                       !counter
             else counter.Value <- !counter
                  !counter
let rec getLast a =
    match a with
    |[] -> Boolean(false)
    |[(c,d)] -> d
    |_::xs -> getLast xs

let rec edgesCD c s e n d =
    match c with
        |INIT(a,b) ->  [(Edge(s,Assignment(a,b),e),d)]
        |TWOcommand(a,b) ->  edgesCD a  s (Node(sprintf "q%i" (n)))  (factory(true)) d @ edgesCD b (Node(sprintf "q%i" (n))) e (factory(false)) d
        |IFFI(a)         ->  edgesGD a s e (factory(false)) (Boolean(false))
        |DOOD(a)         ->  edgesGD a s s (factory(false)) (Boolean(false)) @ [(Edge(s,Baction(Not (getLast (edgesGD a s s (factory(false)) (Boolean(false))))), e),d)]
        |Skip            ->   [(Edge(s,Saction,e),d)]

        

and edgesGD g s e n d =
    match g with        
        |Follows(a,b)   ->  [(Edge(s, Baction(And(a,Not( d))) , (Node(sprintf "q%i" (factory(false))))),d)] @ edgesCD b (Node(sprintf "q%i" (factory(false)))) e (factory(true)) (Or(a,d))
        |TWOguardedC(a,b) -> edgesGD a s e  (factory(false)) d @ edgesGD b s e (factory(false)) (getLast (edgesGD a s e (factory(false)) d))

       
let getNode b = 
    match b with
    |Node(a) -> a

let getLabel d= 
    match d with
    | Assignment(a,b) -> printE(a) + " := " + printE(b)
    | Saction         -> "Skip"
    | Baction(b)         ->  printB(b)



let rec toDotD e dot=
    match e with
    | [] -> dot + "}"
    | (Edge(a,b,c),d)::xs -> toDotD xs (dot + (getNode(a)) + "->" + (getNode(c)) + "[label = \"" + (getLabel(b)) + "\"];\n")
    
