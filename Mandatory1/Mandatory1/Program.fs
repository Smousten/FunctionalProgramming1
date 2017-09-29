

type no = int                                           // tlf nr
type yb = int                                           // year of birth
type ths = string list                                  // themes of interests
type name = string                                      // member name
type mDescription = no * yb * ths                       // member description tuple
type clubMember = name * mDescription                   // pair of member name and description      
type register = clubMember list                         // list of clubmembers

// Register of club members
let reg = [("Tim",(42021021,1996,["soccer";"jazz"]));
           ("Bob",(88888888,1974,["engineering";"explosives"]));
           ("Scott",(12312831,1982,["tennis";"jazz"]));
           ("Andrew",(21931319,1992,["soccer";"basketball"]))]

// predicate 1 is an arrangement for young people interested in both soccer and jazz
let p1 = (112,1982,["soccer";"jazz"])

// predicate 1 is an arrangement for young people interested in either soccer or jazz
let p2 = (113,1982,["soccer";"jazz"])

// auxilliary functions to get member name and description, type for getName: 'a * 'b -> 'a, type for getDesc: 'b -> 'b
let getName (name,_) = name
let getDesc (_,mDescription) = mDescription

// auxilliary functions to get tlf nr, birth year and themes from member description
let tlfno (no,_,_) = no //  type 'a * 'b * 'c -> 'a
let ybirth (_,yb,_) = yb // type: 'a * 'b * 'c -> 'b
let themes (_,_,ths) = ths // type: 'a * 'b * 'c -> 'c

// Auxilliary method to see if same element exists in to lists, type: 'a -> 'a list -> bool
let rec contains y = function 
    | [] -> false
    | x::xs -> x=y || contains y xs
    
// Auxilliary function to compare interests/themes between members, type: 'a list -> bool 
let rec matchPredicateThemes pred desc =
    match pred with 
    | ([]) -> false
    | (x::xtail) -> if (contains x desc) then true
                    else matchPredicateThemes xtail desc

let ageAccepted memAge predAge = memAge > predAge // 'a -> 'a -> bool

// Checks if member qualifies for predicate, type:  'a * 'b * 'c list -> 'd * ('e * 'b * 'c list) -> bool
let isInterested p cMember = (matchPredicateThemes (themes p) (themes (getDesc cMember)) && ageAccepted (ybirth (getDesc cMember)) (ybirth p))

// Identifies the interested member, type: 'a -> 'b * ('c * 'd * 'e) -> 'c * 'b
let interestedMember p cMember = tlfno (getDesc cMember), getName cMember

// The type of extractInterested is 'a * 'b * 'c list -> r:('d * ('e * 'b * 'c list)) list -> ('e * 'd) list
let rec extractInterested p r =
    match r with
    | [] -> []
    | r'::rtail -> if (isInterested p r') then (interestedMember p r')::extractInterested p rtail
                   else extractInterested p rtail

// The expected result of test1 is a pair of tlf nr and name of Tim
let test1 = extractInterested p1 reg

// The expected result of test2 is two pairs of tlf nr and name of Tim and Andrew
let test2 = extractInterested p2 reg
            