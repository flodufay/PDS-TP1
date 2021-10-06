open ASD
open Token

(* This gives some stream parser usage examples. You can discard them once you understood the syntax *)
(* These are high-level generic functions: they take a parser as argument *)
(* p? *)
let opt p = parser
  | [< x = p >] -> Some x
  | [<>] -> None

(* p* *)
let rec many p = parser
  | [< x = p; l = many p >] -> x :: l
  | [<>] -> []

(* p+ *)
let some p = parser
  | [< x = p; l = many p >] -> x :: l

(* Simple example usage: this function count the number of `SEMICOLON' *)
(* Its takes a Stream of token as argument, and gives an integer *)
let parse_semicolon : token Stream.t -> int = parser
  | [< 'SEMICOLON >] -> 1 (* Parsers need to match a `SEMICOLON' token *)

let count_semicolon = parser
  | [< l = many parse_semicolon >] -> List.length l (* Parser needs to match `many' `SEMICOLON' and returns the count *)

let parse_othersubject : token Stream.t -> int = parser
  | [< 'OTHERSUBJECT >] -> 1 

let count_other subject = parser
  | [< l = many parse_othersubject >] -> List.length l 
let parse_add : token Stream.t -> int = parser
  | [< 'ADD >] -> 1 

let count_add = parser
  | [< l = many parse_add >] -> List.length l 

let rec parse p =
  parse_sujet p [];;

let rec parse_sujet p res_sujet = match p with
  | String x::q -> parse_predicat x q res_sujet []
  | [] -> res_sujet

let rec parse_predicat x p res_sujet res_predicat = match p with
  | String y::q -> parse_objet y q res_sujet [] res_predicat

let rec parse_objet x y paprser res_sujet res_objet = match p with
  | String z::ADD::q -> parse_objet x y q res_sujet ObjectTTL(z)::res_objet res_predicat
  | String z::SEMICOLON::q -> parse_predicat x q res_sujet PredicatTTL( y, ObjectTTl(z)::res_objet)::res_predicat
  | String z::OTHERSUBJECT::q -> parse_sujet q SujetTTL(x, PredicatTTL( y, ObjectTTl(z)::res_objet)::res_predicat)::res_sujet
