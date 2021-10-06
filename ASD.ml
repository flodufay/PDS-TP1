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
      |t::q -> t#print_doc ^ "/n" ^ (phraseN(q))#print_doc
      | _ -> ""
end;;


class objetTTL (objet : string) =
  object
    val x = objet
    method transform y z = sujetN y (predicatN z (objetN x)) 
end;;

class predicatTTL (predicat : string) (multiObjet : objetTTL list) =
  object
    val x = predicat
    val l = multiObjet
    method transform y = ( parcours y x l)
    method parcours y x l = match l with
        |t::q -> (t#transform y x)::(parcours y x q)
        |_ -> []
end;;

class sujetTTL (sujet : string) (multiPredicat : predicatTTL list) =
  object
    val x = sujet
    val l = multiPredicat
    method transform : sujetN = ( parcours x l)
    method parcours x l = match l with
        |t::q -> (t#tranform x) :: (parcours x q)
        |_ -> [] 
end;;

class phraseTTL (multiSujet : sujetTTL list) =
  object
    val p = multiSujet
    method transform : phraseN = phraseN (parcours p) 
    method parcours p = match p with
        |t::q -> t#transform :: (parcours q)
        |_ -> []
end;;



(* Function to generate the document out of the AST *)
let rec ntriples_of_ast (ast : phraseTTL) : string =
  (ast#transform)#print_doc;;
