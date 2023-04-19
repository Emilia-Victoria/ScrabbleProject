// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    let add a b = a >>= fun x -> b >>= (fun y -> ret (x + y))     
    let div a b = a >>= fun x -> b >>= fun y -> if y <> 0 then ret (x/y) else fail DivisionByZero     

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =
        match a with
        |N n -> ret n
        |V x -> lookup x
        |WL -> wordLength
        |PV a -> arithEval a >>= fun x -> pointValue x
        |Add (x,y) -> add (arithEval x) (arithEval y)
        |Sub (x,y) -> arithEval x >>= fun x -> arithEval y >>= (fun y -> ret (x - y))
        |Mul (x,y) -> arithEval x >>= fun x -> arithEval y >>= (fun y -> ret (x * y))
        |Div (x,y) -> div (arithEval x) (arithEval y)
        |Mod (x,y) -> arithEval x >>= fun x -> arithEval y >>= (fun y -> if y <> 0 then ret (x % y) else fail DivisionByZero)
        |CharToInt c -> charEval c >>= fun x -> ret (int x)     

    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV a -> arithEval a >>= fun x -> characterValue x
        | ToUpper c -> charEval c >>= fun x -> ret (System.Char.ToUpper x)
        | ToLower c -> charEval c >>= fun x -> ret (System.Char.ToLower x)
        | IntToChar a -> arithEval a >>= fun x -> ret (char x)     

    let isVowel c = List.contains (System.Char.ToLower c) ['a'; 'e'; 'i'; 'o'; 'u'; 'y']
    
    let isLetter c = System.Char.IsLetter c

    let isDigit c = System.Char.IsDigit c
    let rec boolEval b : SM<bool> =
        match b with
        | TT -> ret true                
        | FF -> ret false
        | AEq (x,y) -> arithEval x >>= fun x -> arithEval y >>= (fun y -> ret (x = y))
        | ALt (x,y) -> arithEval x >>= fun x -> arithEval y >>= (fun y -> ret (x < y))
        | Not b -> boolEval b >>= fun x -> ret (not x)
        | Conj (x,y) -> boolEval x >>= fun x -> boolEval y >>= fun y -> ret (x && y) 
        | IsVowel c -> charEval c >>= fun x -> ret (isVowel x)
        | IsConsonant c -> charEval c >>= fun x -> ret (not (isVowel x))
        //| IsLetter c -> charEval c >>= fun x -> ret (isLetter x)     
        //| IsDigit c -> charEval c >>= fun x -> ret (isDigit x)


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"