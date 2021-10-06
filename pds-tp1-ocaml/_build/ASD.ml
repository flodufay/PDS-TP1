(* ASD type *)
(* type document = *)
  (* Fill here! *)






class objetN(objet : string) =
  object
    val x = objet
    method print_doc : string = x
end;;

class predicatN (predicat : string) (objet : objetN)  =
  object
    val x = predicat
    val l = objet
    method print_doc : string = x ^ l#print_doc
end;;

class sujetN (sujet : string) (predicat : predicatN) =
  object
    val x = sujet
    val l = predicat
    method print_doc : string = x ^ l#print_doc
end;;

class phraseN (multiSujet : sujetN list) =
  object
    val p = multiSujet
    method print_doc :  string = match p with
      |t::q -> t#print_doc ^ "/n" ^ (new phraseN(q))#print_doc
      | _ -> ""
end;;


class objetTTL (objet : string) =
  object
    val x = objet
    method transform y z =new  sujetN y (new predicatN z (new objetN x)) 
end;;


let rec parcoursPredicatTTL y x (l : objetTTL list) = match l with
  |t::q -> (t#transform y x)::(parcoursPredicatTTL y x q)
  |_ -> []


class predicatTTL (predicat : string) (multiObjet : objetTTL list) =
  object
    val x = predicat
    val l = multiObjet
    method transform y = ( parcoursPredicatTTL y x l)
        end;;

let rec parcoursSujetTTL x (l : predicatTTL list) = match l with
        |t::q -> (t#transform x) @ (parcoursSujetTTL x q)
        |_ -> [] 


class sujetTTL (sujet : string) (multiPredicat : predicatTTL list) =
  object
    val x = sujet
    val l = multiPredicat
    method transform : sujetN list = (parcoursSujetTTL x l)

end;;

let rec parcoursPhraseTTL (p : sujetTTL list) = match p with
        |t::q -> t#transform @ (parcoursPhraseTTL q)
        |_ -> []

class phraseTTL (multiSujet : sujetTTL list) =
  object
    val p = multiSujet
    method transform : phraseN = new phraseN (parcoursPhraseTTL p) 
end;;








(* Function to generate the document out of the AST *)
let rec ntriples_of_ast (ast : phraseTTL) : string =
  (ast#transform)#print_doc;;
