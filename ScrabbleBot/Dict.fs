module internal Dict
    type Dict<'a when 'a : comparison> =
        | Leaf of bool
        | Node of bool * Map<char,Dict<'a>>
    let empty () = Leaf false
    let rec insert (s:string) =
        function
        | Leaf _ when s.Length = 0 -> Leaf true
        | Node (_, dict) when s.Length = 0 -> Node (true, dict)
        | Leaf b -> Node (b, Map.empty |> Map.add s.[0] (insert s.[1..] (empty())))
        | Node (b, dict) ->
            match Map.tryFind s.[0] dict with
            |None -> Node(b, Map.add s.[0] (insert s.[1..] (empty())) dict)
            //|Some d ->
    let lookup s dict = true
    let step c dict = 0
