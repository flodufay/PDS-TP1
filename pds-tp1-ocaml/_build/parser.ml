open ASD
open Token

(* This gives some stream parser usage examples. You can discard them once you understood the syntax *)
(* These are high-level generic functions: they take a parser as argument *)
(* p? *)



let rec parse_sujet (p : Token.token list) (res_sujet : ASD.sujetTTL list) = 
  let rec parse_predicat x (p : Token.token list) (res_sujet : ASD.sujetTTL list) res_predicat = 
    
    let rec parse_objet x y (p : Token.token list) (res_sujet : ASD.sujetTTL list) res_predicat res_objet =  match p with
      | (STRING z)::ADD::q -> parse_objet x y q res_sujet res_predicat (new ASD.objetTTL(z)::res_objet)
      | (STRING z)::SEMICOLON::q -> parse_predicat x q res_sujet (new ASD.predicatTTL y (new ASD.objetTTL(z)::res_objet)::res_predicat) 
      | (STRING z)::OTHERSUBJECT::q -> parse_sujet q ((new ASD.sujetTTL x ((new ASD.predicatTTL y ((new ASD.objetTTL(z))::res_objet))::res_predicat))::res_sujet) in
  match p with
    | (STRING y)::q -> parse_objet x y q res_sujet res_predicat [] in
match p with
  | (STRING x)::q -> parse_predicat x q res_sujet []
  | [] -> res_sujet;;

let rec parse (p : Token.token list) =
  new ASD.phraseTTL (parse_sujet p []);;
