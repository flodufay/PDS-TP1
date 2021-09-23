class phrase(multiSujet) =
  object
    val p : sujet list
    p = multiSujet
end;;

class sujet(sujet, multiPredicat) =
  object
    val x : string
    x = "<" + sujet ">"
    val l : predicat list
    l = multiPredicat
end;;

class predicat(predicat, multiObjet) =
  object
    val x : string
    x = "<" + predicat + ">"
    val l : objet list
    l = multiObjet
end;;

class objet(objet) =
  object
    val x = objet
end;;