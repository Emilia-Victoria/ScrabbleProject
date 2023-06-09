module Dict

    open System.Collections.Generic
    type Dictionary = 
        | Leaf of bool
        | Node of bool * Dictionary<char, Dictionary>
    let empty () = Leaf false

    let rec insert (s:string) =
        function
        | Leaf _ when s.Length = 0 -> 
            Leaf true
        | Node (_, dict) when s.Length = 0 -> 
            Node(true, dict)

        | Leaf b ->
            let dict = Dictionary ()
            dict.[s.[0]] <- insert s.[1..] (empty ())
            
            Node (b, dict)  

        | Node (b, dict) ->
            match dict.TryGetValue s.[0] with
            | (true, d) ->
                dict.[s.[0]] <- insert s.[1..] d
                Node (b, dict)
            | (false, _) ->
                dict.[s.[0]] <- insert s.[1..] (empty ())
                Node (b, dict)

    let rec lookup (s:string) =
        function
        | Leaf b when s.Length = 0 -> b
        | Leaf _ -> false
        | Node (b, _) when s.Length = 0 -> b
        | Node (_, dict) ->
            match dict.TryGetValue s.[0] with
            | (true, d) -> lookup s.[1..] d
            | (false, _) -> false      

    let rec step c =
        function 
        | Node (_, dict) -> 
            match dict.TryGetValue c with 
            | (true, value) ->
                match value with
                | Leaf b -> Some (b, value)
                | Node (b,_) -> Some (b,value)
            | (false, _) -> None
        | Leaf _ -> None
