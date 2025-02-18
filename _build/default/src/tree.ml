type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

let rec tree_fold f init tree = match tree with
| Leaf -> init
| Node(left,treeVal,right) -> f (tree_fold f init left) treeVal (tree_fold f init right) 




let map tree f = tree_fold (fun left treeVal right ->
Node( left,(f treeVal),right ) 
) Leaf tree


let mirror tree = tree_fold (fun left treeVal right ->
Node(right,treeVal,left) 
) Leaf tree




let in_order tree = tree_fold (fun left treeVal right ->
left @ [treeVal] @ right
) [] tree




let pre_order tree = tree_fold (fun left treeVal right ->
[treeVal] @ left @ right
) [] tree


let composeHelp funcList x = List.fold_left (fun a h -> (h a) ) x funcList 


let compose tree x = composeHelp (in_order tree) x

let depth tree = tree_fold (fun left treeVal right ->
(max left right) + 1
) 0 tree



let idxTree tree = tree_fold (fun left treeVal right ->
match left with
(idx,tree1) -> match right with
(idx2,tree2) -> ( (max idx idx2)+1,Node(tree1,((max idx idx2)+1,treeVal),tree2))
) (0,Leaf) tree


let unIdxTree tree = map tree (fun x -> match x with
(idx,treeVal) -> treeVal
) 


let indexTup lst = let result = (List.fold_left (fun (idx,tup) elem  -> ((idx+1,(idx,elem)::tup) ) ) (1,[]) lst) in 
match result with (a,b) -> List.rev b

let unIdxTup lst = List.map (fun x -> match x with
(idx,tupVal) -> tupVal
) lst

(* Assume complete tree *)


let trim tree n = unIdxTree ( let cutoff = (depth tree) - n in
tree_fold (fun left treeVal right -> match treeVal with
(idx,actualVal) -> if (idx <= cutoff) then Leaf
else Node(left,treeVal,right)
) Leaf ( match (idxTree tree) with (a,b) -> b ) )



let rec tree_init f v1 v2 = match (f v1 v2) with 
| Some ((a,b),v2,(c,d)) -> Node((tree_init f a b ),v2,(tree_init f c d) )
| None -> Leaf


let rec splitHelp lst v acc = match lst with
[] -> ([],[])
| curr::rest -> if (curr = v) then 
(acc,rest)
else splitHelp rest v (acc @ [curr])

let rec splitHelp2 lst v acc = match lst with
[] -> ([],[])
| curr::rest -> match curr with (a,b) -> if (a=v) then
(acc @ [curr],rest)
else splitHelp2 rest v (acc @ [curr])


let rec countLst lst = match lst with
[] -> 0
| curr::rest -> 1 + countLst rest


let rec split lst v = splitHelp lst v []

let rec split2 lst v = match (splitHelp2 (indexTup lst) v []) with (a,b) ->
( (unIdxTup a),(unIdxTup b) )




let treeG a b = match a with
[] -> None
| curr::rest -> match (split b curr) with (lst1,lst2) ->
  match (split2 rest (countLst lst1)) with (lst3,lst4) ->
Some((lst3,lst1),curr,(lst4,lst2))




 

let from_pre_in pre in_ord = tree_init treeG pre in_ord




