type person = { name : string; age : int; hobbies : string list }
type comparator = person -> person -> int
type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

(* TODO: Implement functions below *)

type db  = {personData: person list}

let newDatabase = {personData=[]}

let insert person db = {personData=(person::db.personData)}

let rec rmvPerson lst personName = match lst with
[] -> [] 
| curr::rest -> 
if curr.name = personName then (rmvPerson rest personName)
else curr::(rmvPerson rest personName)

let remove name db = {personData=(rmvPerson db.personData name)}

let sort comparator db = (List.sort comparator db.personData)



let rec evalCon condition person = match condition with
True -> true
| False -> false
| Age ageFunc -> (ageFunc person.age)
| Name nameFunc -> (nameFunc person.name) 
| Hobbies hobFunc -> (hobFunc person.hobbies)
| And (con1,con2) -> (evalCon con1 person) && (evalCon con2 person) 
| Or (con1,con2) -> (evalCon con1 person) || (evalCon con2 person) 
| Not con -> not (evalCon con person)
| If (con1,con2,con3) -> if (evalCon con1 person) then (evalCon con2 person) else (evalCon con3 person)



let rec conPerson lst condition = match lst with
[] -> [] 
| curr::rest -> 
if (evalCon condition curr) then curr::(conPerson rest condition)
else (conPerson rest condition)

let query condition db = conPerson db.personData condition




let queryBy condition db comparator = (query condition {personData=(sort comparator db)})

let update condition db change = {personData=(List.map (fun x -> if (evalCon condition x) then (change x) else x) db.personData)}


let rec delePerson lst condition = match lst with
[] -> [] 
| curr::rest -> 
if (evalCon condition curr) then (delePerson rest condition)
else curr::(delePerson rest condition)




let deleteAll condition db = {personData=(delePerson db.personData condition)}
