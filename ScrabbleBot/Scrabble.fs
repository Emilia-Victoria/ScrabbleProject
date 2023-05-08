namespace EmyIsGr8tttt

open MultiSet
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open Parser
open StateMonad
// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        playedTiles   : Map<coord, char>
    }

    let mkState b d pn h pt = {board = b; dict = d;  playerNumber = pn; hand = h; playedTiles = pt}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let playedTiles st     = st.playedTiles 

module Scrabble =
    open System.Threading
    open State
    open MultiSet

    let updateHand (hand: MultiSet.MultiSet<uint32>) (usedTiles: uint32 list) (newTiles: (uint32*uint32) list) : MultiSet.MultiSet<uint32> =
        let handAfterRemove = List.fold (fun (h:MultiSet<uint32>) (a:uint32)-> MultiSet.removeSingle a h) hand usedTiles
        let handAfterInsert = List.fold (fun (h:MultiSet<uint32>) (a:uint32*uint32)-> MultiSet.add (fst a) (snd a) h) handAfterRemove newTiles
        handAfterInsert
    
    let handToIDLst (hand :MultiSet.MultiSet<uint32>): uint32 list = MultiSet.fold (fun acc key _ -> acc @ [key] ) List.Empty hand
    let bestWord l1 l2 = if List.length l1 > List.length l2 then l1 else l2
    
    type Direction =
        |Right
        |Down
    
    let nextCoord (x,y) dir =
        match dir with
        | Right -> (x+1,y)
        | Down -> (x, y+1)
    
    let isStartSurrounded (x,y) dir st =
        match dir with
        | Right ->
            match Map.tryFind (x-1, y) st.playedTiles with
            | Some _ -> true
            | None ->
                match Map.tryFind (x+1, y) st.playedTiles with
                | Some _ -> true
                | None -> false
        | Down ->
            match Map.tryFind (x, y-1) st.playedTiles with
            | Some _ -> true
            | None ->
                match Map.tryFind (x, y+1) st.playedTiles with
                | Some _ -> true
                | None -> false
    
    let isSurrounded (x,y) dir st =
        match dir with
        | Right ->
            match Map.tryFind (x+1, y) st.playedTiles with
            | Some _ -> true
            | None ->
                match Map.tryFind (x,y+1) st.playedTiles with
                | Some _ -> true
                | None ->
                    match Map.tryFind (x, y-1) st.playedTiles with
                    | Some _ -> true
                    | None -> false
        | Down ->
            match Map.tryFind (x+1, y) st.playedTiles with
            | Some _ -> true
            | None ->
                match Map.tryFind (x-1,y) st.playedTiles with
                | Some _ -> true
                | None ->
                    match Map.tryFind (x, y+1) st.playedTiles with
                    | Some _ -> true
                    | None -> false
    
  
        
    let makeMove coord dir st pieces =
                let rec aux dict hand curWord longestWord coord dir =
                    match Map.tryFind coord st.playedTiles with
                    | None -> 
                        MultiSet.fold (fun word id _ ->
                                let updatedHand = removeSingle id hand
                                let tile = Map.find id pieces |> Set.toList |> List.head
                                if isSurrounded coord dir st then word else
                                let c = fst tile
                                match Dictionary.step c dict with
                                | None -> 
                                    word
                                | Some (false,tempDict) -> 
                                    //if not a word, continue
                                    let curWordUpdated = curWord @ [(coord,(id,tile))]
                                    aux tempDict updatedHand curWordUpdated word (nextCoord coord dir) dir
                                | Some(true,tempDict) ->
                                    //return curWord if is a valid word
                                    let curWordUpdated = curWord @ [(coord,(id,tile))]
                                    let longest = bestWord curWordUpdated longestWord 
                                    aux tempDict updatedHand curWordUpdated longest (nextCoord coord dir) dir
                        ) longestWord hand
                    | Some cv ->
                        match Dictionary.step cv dict with
                        | None -> longestWord
                        | Some (_,tempDict) -> aux tempDict hand curWord longestWord (nextCoord coord dir) dir
                aux st.dict st.hand List.Empty List.Empty coord dir
    
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            
            //let rec FindWord (hand: st.hand) (curWord: string) : String =
            //    match hand with
            //    | hand -> ScrabbleUtil.Dictionary.step   
            //    | _ -> if (ScrabbleUtil.Dictionary.lookup curWord = true) then curWord else FindWord st.hand curWord 

            let convertIdToChar (id: uint32) = char(int32(id) + int32('A') - 1)
            let convertCharToId (c: char) = uint32(int32(c) - int32('A') + 1)   

            //let charHand hand : List<char> = (List.fold (fun acc tile -> (convertIdToChar tile) :: acc) List.empty (toList hand))        

            let chooseMove = ""
            //let move = RegEx.parseMove input
            let move =
                
                if Map.isEmpty st.playedTiles
                then
                    let moveRight = makeMove (0,0) Right st pieces
                    let moveDown = makeMove (0,0) Down st pieces
                    bestWord moveRight moveDown
                else
                    let moveRight = Map.fold (fun acc key _ -> if isStartSurrounded key Right st then acc else makeMove key Right st pieces |> bestWord acc) [] st.playedTiles
                    let moveDown = Map.fold (fun acc key _ -> if isStartSurrounded key Down st then acc else makeMove key Down st pieces |> bestWord acc) [] st.playedTiles
                    bestWord moveRight moveDown

            
            //Check if center is occupied. If not find longest word we can make from our hand and place it.
            //If occupied find longest word we can make taking the board into account.

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (
                if List.isEmpty move
                then
                    if MultiSet.size st.hand < 7u
                    then SMPass
                    else
                        //if int(st.amountTiles - (MultiSet.size st.hand) - uint32(Map.count st.playedTiles)) < 7
                        //then
                        //SMPass
                        //else
                        SMChange (MultiSet.toList st.hand)
                        
                else SMPlay move)
            
            
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = mkState st.board st.dict st.playerNumber (
                    //St.Hand
                    let listOfID: uint32 list = List.fold (fun (acc: uint32 list) (_,(b,(_,_))) -> acc @ [b]) List.Empty ms
                    let listOfNewPiecesID: (uint32*uint32) list = List.fold (fun (acc: (uint32*uint32) list) a -> acc @ [a]) List.Empty newPieces
                    updateHand st.hand listOfID listOfNewPiecesID) (
                    //St.PlayedTiles
                    List.fold (fun (acc)  (a,(_,(c,_))) -> Map.add a c acc) st.playedTiles ms)
                
                aux st'
            | RCM (CMChangeSuccess newTiles) ->
                let st' = mkState st.board st.dict st.playerNumber (updateHand st.hand (MultiSet.toList st.hand) newTiles) st.playedTiles
                aux st'
                
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPassed _) -> aux st
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> //If notENoughPieces -> Pass
            aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        let pTiles = Map.empty
 
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet pTiles)
        