﻿type no = int                                           // tlf nr
type yb = int                                           // year of birth
type ths = string list                                  // themes of interests
type name = string                                      // member name
type mDescription = no * yb * ths                       // member description tuple
type clubMember = name * mDescription                   // pair of member name and description      
type register = clubMember list                         // list of clubmembers

// Register of club members
let reg = [("Tim",(42021021,1996,["soccer";"jazz"]));
           ("Bob",(88888888,1974,["engineering";"building";"explosions"]));
           ("Scott",(12312831,1982,["tennis";"jazz";"games"]));
           ("Andrew",(21931319,1992,["soccer";"basketball";"cake"]));
           ("Bill",(28913453,1955,["science";"kids";"tv";"explosions"]));
           ("Dovahkiin",(13371337,2011,["dragons";"loot";"dungeons";"explosions";"smithing";"running";"riding";"games"]));
           ("Søren Pilmark",(295435363,1919,["song";"theater";"acting";"dance";"tv"]));
           ("Timmy",(24353462,2006,["games";"crendor"]));
           ("Boar Thrills",(343253463,2006,["survival";"nature";"urine"]))
           ]
           

// auxilliary functions to get member name and description, type for getName: 'a * 'b -> 'a, type for getDesc: 'b -> 'b
let getName (name,_) = name
    
// auxilliary functions to get tlf nr, birth year and themes from member description
let tlfno (no,_,_) = no //  type 'a * 'b * 'c -> 'a
let ybirth (_,yb,_) = yb // type: 'a * 'b * 'c -> 'b
let themes (_,_,ths) = ths // type: 'a * 'b * 'c -> 'c
let getDesc (_,mDescription) = mDescription 


// Auxilliary method to see if same element exists in two lists, type: 'a -> 'a list -> bool
let rec contains y = function
    | [] -> false
    | x::xs -> x=y || contains y xs

 // 'a -> 'a -> bool

// Auxilliary method to see if elements exists in both lists
let ageAccepted memAge predAge = memAge > predAge

//let matchesN noOfMatches member1 member2 = for i in 0..noOfMatches do if (contains (themes (getDesc member1))  (themes member2)) then 

let matchesN themesMember themesPred = 
    let mutable n = 0 
    for i in (themesPred) do
        if (contains i themesMember) 
            then n <- n + 1
    n
        

// predicate 1 is an arrangement for young people interested in both soccer and jazz

let predicate arrangment cMember noOfMatches = 
    if (ageAccepted (ybirth (getDesc cMember)) (ybirth (getDesc arrangment))) then
        if (matchesN (themes (getDesc cMember)) (themes (getDesc arrangment)) >= noOfMatches) then
            true
        else
            false
    else
        false


// predicate 1 is an arrangement for young people interested in either soccer or jazz
//let p2 = (113,1982,["soccer";"jazz"]) // Either


    
// Auxilliary function to compare interests/themes between members, type: 'a list -> bool 
//let rec matchPredicateThemes pred desc =
//    match pred with 
//    | ([]) -> false
//    | (x::xtail) -> if (contains x desc) then true
//                    else matchPredicateThemes xtail desc



// Checks if member qualifies for predicate, type:  'a * 'b * 'c list -> 'd * ('e * 'b * 'c list) -> bool
//let isInterested p cMember = (matchPredicateThemes (themes p) (themes (getDesc cMember)) && ageAccepted (ybirth (getDesc cMember)) (ybirth p))

// Identifies the interested member, type: 'a -> 'b * ('c * 'd * 'e) -> 'c * 'b
//let interestedMember p cMember = tlfno (getDesc cMember), getName cMember

let rec allInterested arrangment reg noOfMatches =
    match reg with 
    | [] -> []
    | reg'::regtail -> if (predicate arrangment reg' noOfMatches) then (tlfno (getDesc reg'),getName reg')::allInterested arrangment regtail noOfMatches
                       else allInterested arrangment regtail noOfMatches
   

// The type of extractInterested is 'a * 'b * 'c list -> r:('d * ('e * 'b * 'c list)) list -> ('e * 'd) list
let extractInterested arrangment cMemberReg noOfMatches =
   let interestedMembers = List.append (allInterested arrangment cMemberReg noOfMatches) []
   interestedMembers

// ///////////////////////////////////////////////////////////////////////////
// |-------------------------- Whitebox Tests -------------------------------|
// \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

// The expected result of test1 is a pair of tlf nr and name of Tim
let test1 = extractInterested ("Jazz 'n' Football",(112,1982,["soccer";"jazz"])) reg 2 =[(42021021, "Tim")]

// The expected result of test2 is two pairs of tlf nr and name of Tim and Andrew
let test2 = extractInterested ("Jazz or Football",(991,1982,["soccer";"jazz"])) reg 1 = [(42021021, "Tim"); (21931319, "Andrew")]

// The expected result of test3 is three pairs of tlf nr and name of Andrew, Bill and Søren Pilmark
let test3 = extractInterested ("Netflix Marathon",(999,1890,["tv";"cake"])) reg 1 = [(21931319, "Andrew"); (28913453, "Bill"); (295435363, "Søren Pilmark")]

// The expected result of test4 is three pairs of tlf nr and name of Timmy and Dovahkiin
let test4 = extractInterested ("Lan Party",(666,1890,["tv";"games";"crendor";"dungeons"])) reg 2 = [(13371337, "Dovahkiin"); (24353462, "Timmy")]

// The expected result of test5 is three pairs of tlf nr and name of Bob and Bill
let test5 = extractInterested ("Science and Engineering Convention",(1942,1890,["explosions";"science";"engineering"])) reg 2 = [(88888888, "Bob"); (28913453, "Bill")] 

// The expected result of test6 is three pairs of tlf nr and name of Bob and Bill
let test6 = extractInterested ("Public Service Announcement",(1800,-1,["emergency"])) reg 0 = [(42021021,"Tim"); (88888888,"Bob"); (12312831,"Scott"); (21931319,"Andrew"); (28913453,"Bill"); (13371337,"Dovahkiin"); (295435363,"Søren Pilmark"); (24353462,"Timmy"); (343253463,"Boar Thrills")]