

type no = int                                           // tlf nr
type yb = int                                           // year of birth
type ths = string list                                  // themes of interests
type name = string                                      // member name
type mDescription = no * yb * ths                       // member description tuple
type clubMember = (name * mDescription)                 // pair of member name and description      
type register = clubMember list                         // list of clubmembers
type arrang = bool                                      // 

let reg = [("Tim",(42021021,1996,["soccer";"jazz"]));
           ("Bob",(88888888,1974,["engineering";"explosives"]));
           ("Scott",(12312831,1982,["tennis";"jazz"]))]

let p1 = (112,1982,["soccer";"jazz"])
let p2 = (113,1982,["soccer";"jazz"])

let tlfno (no,_,_) = no
let ybirth (_,yb,_) = yb
let themes (_,_,ths) = ths



let rec extractInterested p r = function
    | (0,_) -> []
    | (_,[]) -> []
    | (p,r) -> 
