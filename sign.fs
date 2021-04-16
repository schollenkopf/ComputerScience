module sign


type sign_type =
    |Plus
    |Minus
    |Zero

type sign_bool =
    |TT
    |FF

let sign_plus a b =
    match (a,b) with
    |(Zero,Zero) -> [Zero]
    |(Plus,Zero) -> [Plus]
    |(Zero,Plus) -> [Plus]
    |(Minus,Zero) -> [Minus]
    |(Zero,Minus) -> [Minus]
    |(Minus,Minus) -> [Minus]
    |(Plus,Plus) -> [Plus]
    |(Plus,Minus) -> [Minus;Plus;Zero]
    |(Minus,Plus) -> [Minus;Plus;Zero]

let rec sign_plus_eval list1 list2 =
    match list1 with
    |a::xs -> plus_helper a list2 xs
    |[] -> []
and plus_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_plus a b)@(plus_helper a xa xs)
    |[] -> sign_plus_eval xs list2

let sign_Minus a b =
    match (a,b) with
    |(Zero,Zero) -> [Zero]
    |(Plus,Zero) -> [Plus]
    |(Zero,Plus) -> [Minus]
    |(Minus,Zero) -> [Minus]
    |(Zero,Minus) -> [Plus]
    |(Minus,Minus) -> [Minus;Plus;Zero]
    |(Plus,Plus) -> [Minus;Plus;Zero]
    |(Plus,Minus) -> [Plus]
    |(Minus,Plus) -> [Minus]

let rec sign_minus_eval list1 list2 =
    match list1 with
    |a::xs -> minus_helper a list2 xs
    |[] -> []
and minus_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_Minus a b)@(minus_helper a xa xs)
    |[] -> sign_minus_eval xs list2



let sign_Times a b =
    match (a,b) with
    |(Zero,_) -> [Zero]
    |(_,Zero) -> [Zero]
    |(Minus,Minus) -> [Plus]
    |(Plus,Plus) -> [Plus]
    |(Plus,Minus) -> [Minus]
    |(Minus,Plus) -> [Minus]

let rec sign_times_eval list1 list2 =
    match list1 with
    |a::xs -> times_helper a list2 xs
    |[] -> []
and times_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_Times a b)@(times_helper a xa xs)
    |[] -> sign_times_eval xs list2

let sign_Division a b =
    match (a,b) with
    |(Zero,_) -> [Zero]
    |(_,Zero) -> failwith"Division by Zero"
    |(Minus,Minus) -> [Plus]
    |(Plus,Plus) -> [Plus]
    |(Plus,Minus) -> [Minus]
    |(Minus,Plus) -> [Minus]

let rec sign_division_eval list1 list2 =
    match list1 with
    |a::xs -> division_helper a list2 xs
    |[] -> []
and division_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_Division a b)@(division_helper a xa xs)
    |[] -> sign_division_eval xs list2

let sign_Power a b =
    match (a,b) with
    |(Zero,_) -> [Zero]
    |(_,Zero) -> [Plus]
    |(Minus,Minus) -> [Plus;Minus]
    |(Plus,Plus) -> [Plus]
    |(Plus,Minus) -> [Plus]
    |(Minus,Plus) -> [Plus;Minus]

let rec sign_power_eval list1 list2 =
    match list1 with
    |a::xs -> power_helper a list2 xs
    |[] -> []
and power_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_Power a b)@(power_helper a xa xs)
    |[] -> sign_power_eval xs list2

let rec sign_umins_eval list1 =
    match list1 with
    |Plus::xs -> Minus::sign_umins_eval xs
    |Minus::xs -> Plus::sign_umins_eval xs
    |Zero::xs -> Zero::sign_umins_eval xs
    |[] -> []







let rec sign_expr expr mem1 mem2 =
    match expr with
    | Num(a)  -> if a>0 then [Plus] 
                 else if a=0 then [Zero] 
                      else [Minus]
    | Name(a)    -> [Map.find a mem1]      
    | NAMEArray(s,i)   ->  Set.toList(Map.find s mem2)
    | TimesExpr(a,b)   -> Set.toList(Set.ofList(sign_times_eval (sign_expr a mem1 mem2) (sign_expr b mem1 mem2) ))
    | DivExpr(a,b)     -> Set.toList(Set.ofList(sign_division_eval (sign_expr a mem1 mem2) (sign_expr b mem1 mem2) ))
    | PlusExpr(a,b)     -> Set.toList(Set.ofList(sign_plus_eval (sign_expr a mem1 mem2) (sign_expr b mem1 mem2) ))
    | MinusExpr(a,b)   -> Set.toList(Set.ofList(sign_minus_eval (sign_expr a mem1 mem2) (sign_expr b mem1 mem2) ))
    | PowExpr(a,b)     -> Set.toList(Set.ofList(sign_power_eval (sign_expr a mem1 mem2) (sign_expr b mem1 mem2) ))
    | UPlusExpr(a)  -> sign_expr a mem1 mem2
    | UMinusExpr(a)  ->  Set.toList(Set.ofList(sign_umins_eval (sign_expr a mem1 mem2)))




let sign_greater a b =
    match (a,b) with
    |(Zero,Zero) -> [FF]
    |(Plus,Zero) -> [TT]
    |(Zero,Plus) -> [FF]
    |(Minus,Zero) -> [FF]
    |(Zero,Minus) -> [TT]
    |(Minus,Minus) -> [TT;FF]
    |(Plus,Plus) -> [TT;FF]
    |(Plus,Minus) -> [TT]
    |(Minus,Plus) -> [FF]

let rec sign_greater_eval list1 list2 =
    match list1 with
    |a::xs -> greater_helper a list2 xs
    |[] -> []
and greater_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_greater a b)@(greater_helper a xa xs)
    |[] -> sign_greater_eval xs list2

let sign_greaterequal a b =
    match (a,b) with
    |(Zero,Zero) -> [TT]
    |(Plus,Zero) -> [TT]
    |(Zero,Plus) -> [FF]
    |(Minus,Zero) -> [FF]
    |(Zero,Minus) -> [TT]
    |(Minus,Minus) -> [TT;FF]
    |(Plus,Plus) -> [TT;FF]
    |(Plus,Minus) -> [TT]
    |(Minus,Plus) -> [FF]

let rec sign_greaterequal_eval list1 list2 =
    match list1 with
    |a::xs -> greaterequal_helper a list2 xs
    |[] -> []
and greaterequal_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_greaterequal a b)@(greaterequal_helper a xa xs)
    |[] -> sign_greaterequal_eval xs list2

let sign_lower a b =
    match (a,b) with
    |(Zero,Zero) -> [FF]
    |(Plus,Zero) -> [FF]
    |(Zero,Plus) -> [TT]
    |(Minus,Zero) -> [TT]
    |(Zero,Minus) -> [FF]
    |(Minus,Minus) -> [TT;FF]
    |(Plus,Plus) -> [TT;FF]
    |(Plus,Minus) -> [FF]
    |(Minus,Plus) -> [TT]

let rec sign_lower_eval list1 list2 =
    match list1 with
    |a::xs -> lower_helper a list2 xs
    |[] -> []
and lower_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_lower a b)@(lower_helper a xa xs)
    |[] -> sign_lower_eval xs list2

let sign_lowerequal a b =
    match (a,b) with
    |(Zero,Zero) -> [TT]
    |(Plus,Zero) -> [FF]
    |(Zero,Plus) -> [TT]
    |(Minus,Zero) -> [TT]
    |(Zero,Minus) -> [FF]
    |(Minus,Minus) -> [TT;FF]
    |(Plus,Plus) -> [TT;FF]
    |(Plus,Minus) -> [FF]
    |(Minus,Plus) -> [TT]

let rec sign_lowerequal_eval list1 list2 =
    match list1 with
    |a::xs -> lowerequal_helper a list2 xs
    |[] -> []
and lowerequal_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_lowerequal a b)@(lowerequal_helper a xa xs)
    |[] -> sign_lowerequal_eval xs list2

let sign_equal a b =
    match (a,b) with
    |(Zero,Zero) -> [TT]
    |(Plus,Zero) -> [FF]
    |(Zero,Plus) -> [FF]
    |(Minus,Zero) -> [FF]
    |(Zero,Minus) -> [FF]
    |(Minus,Minus) -> [TT;FF]
    |(Plus,Plus) -> [TT;FF]
    |(Plus,Minus) -> [FF]
    |(Minus,Plus) -> [FF]

let rec sign_equal_eval list1 list2 =
    match list1 with
    |a::xs -> equal_helper a list2 xs
    |[] -> []
and equal_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_equal a b)@(equal_helper a xa xs)
    |[] -> sign_equal_eval xs list2

let sign_and a b =
    match (a,b) with
    |(TT,FF) -> [FF]
    |(FF,TT) -> [FF]
    |(TT,TT) -> [TT]
    |(FF,FF) -> [FF]


let rec sign_and_eval list1 list2 =
    match list1 with
    |a::xs -> and_helper a list2 xs
    |[] -> []
and and_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_and a b)@(and_helper a xa xs)
    |[] -> sign_and_eval xs list2

let sign_or a b =
    match (a,b) with
    |(TT,FF) -> [TT]
    |(FF,TT) -> [TT]
    |(TT,TT) -> [TT]
    |(FF,FF) -> [FF]


let rec sign_or_eval list1 list2 =
    match list1 with
    |a::xs -> or_helper a list2 xs
    |[] -> []
and or_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_or a b)@(or_helper a xa xs)
    |[] -> sign_or_eval xs list2


let sign_notequal a b =
    match (a,b) with
    |(Zero,Zero) -> [FF]
    |(Plus,Zero) -> [TT]
    |(Zero,Plus) -> [TT]
    |(Minus,Zero) -> [TT]
    |(Zero,Minus) -> [TT]
    |(Minus,Minus) -> [TT;FF]
    |(Plus,Plus) -> [TT;FF]
    |(Plus,Minus) -> [TT]
    |(Minus,Plus) -> [TT]

let rec sign_notequal_eval list1 list2 =
    match list1 with
    |a::xs -> notequal_helper a list2 xs
    |[] -> []
and notequal_helper a list2 xs =
    match list2 with
    |b::xa -> (sign_notequal a b)@(notequal_helper a xa xs)
    |[] -> sign_notequal_eval xs list2

let rec sign_not_eval list1 =
    match list1 with
    |TT::xs -> FF::sign_not_eval xs
    |FF::xs -> TT::sign_not_eval xs
    |[] -> []


let rec sign_bool c mem1 mem2 =
    match c with
        |Boolean(a) -> if a then [TT] else [FF]
        |And(a,b)   ->  Set.toList(Set.ofList(sign_and_eval (sign_bool a mem1 mem2) (sign_bool b mem1 mem2)))
        |Andand(a,b)   -> Set.toList(Set.ofList(sign_and_eval (sign_bool a mem1 mem2) (sign_bool b mem1 mem2)))
        |Or(a,b)   -> Set.toList(Set.ofList(sign_or_eval (sign_bool a mem1 mem2) (sign_bool b mem1 mem2)))
        |Oror(a,b)   -> Set.toList(Set.ofList(sign_or_eval (sign_bool a mem1 mem2) (sign_bool b mem1 mem2)))
        |Equal(a,b)  -> Set.toList(Set.ofList(sign_equal_eval (sign_expr a mem1 mem2) (sign_expr b mem1 mem2)))
        |NOTEqual(a,b)  -> Set.toList(Set.ofList(sign_notequal_eval (sign_expr a mem1 mem2) (sign_expr b mem1 mem2)))
        |Not(a)         -> Set.toList(Set.ofList(sign_not_eval (sign_bool a mem1 mem2)))
        |GREATER(a,b)  -> Set.toList(Set.ofList(sign_greater_eval (sign_expr a mem1 mem2) (sign_expr b mem1 mem2)))
        |LESS(a,b)  -> Set.toList(Set.ofList(sign_lower_eval (sign_expr a mem1 mem2) (sign_expr b mem1 mem2)))
        |GREATEREqual(a,b)  -> Set.toList(Set.ofList(sign_greaterequal_eval (sign_expr a mem1 mem2) (sign_expr b mem1 mem2)))
        |LESSEqual(a,b)  -> Set.toList(Set.ofList(sign_lowerequal_eval (sign_expr a mem1 mem2) (sign_expr b mem1 mem2)))

let rec createMapsArray a signlist mem1 mem2=
    match signlist with
    |x::xs -> (mem1,(Map.add a (Set.union (Map.find a mem2) (Set.ofList ([x]) )  ) mem2))::createMapsArray a xs mem1 mem2
    |[] -> []

let rec createMapsVariable a signlist mem1 mem2=
    match signlist with
    |x::xs -> (((Map.add a (x) mem1)),mem2)::createMapsVariable a xs mem1 mem2
    |[] -> []

let rec checkAll a =
    match a with
    |x::xs -> if (x=TT) then true
              else checkAll xs
    |[]    -> false

let rec change_sign_memory action mem1 mem2= 
    match action with 
    |Assignment(NAMEArray(a,c),b) ->   createMapsArray a (sign_expr b mem1 mem2) mem1 mem2
    |Assignment(Name (a),b) -> createMapsVariable a (sign_expr b mem1 mem2) mem1 mem2
    |Saction         -> [(mem1,mem2)]
    |Baction(b)         -> if (checkAll(sign_bool b mem1 mem2)) then [(mem1,mem2)] else []
    |_ -> failwith "this action doesnt exist"

let rec semantics edge mem=
    match mem with
    |(a,b)::xs -> (change_sign_memory edge a b)@semantics edge xs
    |[] -> [] 




let rec sign_init_map q qstart map initialSigns=
    match q with
    |x::xs when x=qstart -> sign_init_map xs qstart (Map.add x initialSigns map) initialSigns
                            
    |x::xs ->  sign_init_map xs qstart (Map.add x Set.empty map) initialSigns
               
    |[] -> map

let getTail a  = 
    match a with
    |x::xs -> xs

let rec sign_newmap map x edges w =
    match edges with
    |(n1,a,n2)::xs when n1=x -> let s = Set.ofList(semantics a (Set.toList(Map.find n1 map)))
                                if not(Set.isSubset s (Map.find n2 map) ) then sign_newmap (Map.add n2 (Set.union s (Map.find n2 map) ) map) x xs (w@[n2])
                                else sign_newmap map x xs w
    |(n1,a,n2)::xs      -> sign_newmap map x xs w
    |[] -> (map,w)

let rec sign_while map w edges=
    match w with
    |x::xs -> 
              let a = fst(sign_newmap map x edges w)
              let b = getTail (snd(sign_newmap map x edges w))
              sign_while a b edges
    |[] -> map




let rec sign_getq edges =
    match edges with
    |(n1,a,n2)::xs -> n1::n2::sign_getq xs
    |[] -> []
//@Map.toList(b)

let rec print_set_map list =
    match list with
    |(a,b)::xs -> (a,Set.toList (b))::print_set_map xs
    |[] -> []

let rec sign_print_set l =
    match l with
    |(a,b)::xs -> printfn "%A" (Map.toList(a))
                  printfn "%A" (print_set_map (Map.toList(b)))
                  sign_print_set xs
    |[] -> printfn "Node end"

let rec sign_print map q =
    match q with
    |x::xs -> printfn "%s" (x)
              sign_print_set (Set.toList(Map.find x map))
              sign_print map xs
    |[] -> printfn "Map end"




let sign_main qstart edges initialSigns=
    let m = sign_while (sign_init_map (Set.toList(Set.ofList(sign_getq edges))) qstart Map.empty initialSigns) [qstart] edges
    sign_print m (Set.toList(Set.ofList(sign_getq edges)))