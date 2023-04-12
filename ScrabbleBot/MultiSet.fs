// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a :comparison> = M of Map<'a, uint32>
    let empty = M Map.empty
    let isEmpty (M s) = Map.isEmpty s
    let size (M s) = Map.fold (fun acc key value -> acc + value) 0u s
    let contains a (M s) = Map.containsKey a s
    let numItems a (M s) = if (Map.containsKey a s) then (Map.find a s) else uint32(0)
    let add a n (M s) = M(Map.add a (numItems a (M s) + n) s)
    let addSingle a (M s) = M(Map.add a (uint32(1)) s)
    let remove a n (M s) = if ((numItems a (M s)) < n)
                           then M(Map.remove a s)
                           else M(Map.add a (numItems a (M s) - n) s)
    let removeSingle a (M s) = if contains a (M s)
                               then remove a 1u (M s)
                               else (M s)                           
    let fold f acc (M s) = Map.fold f acc s
    
    let foldBack f (M s) acc = Map.foldBack f s acc
