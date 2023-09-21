module Move where


--formater la liste des mouvements restants 
--unword concatene plusieurs liste en une seule
formaterListeMov :: [[String]] -> String
formaterListeMov listes =
    let formatLigne ligne = unwords ligne
        formatListe liste = unwords liste
    in unwords [formatListe liste | liste <- listes]


let listeBUser = ["X3","X2","X1","X0"]
    let listeMUser = ["X3","X2","X1","X0"]
    let listeSUser = ["X3","X2","X1","X0"]
    let listeMovRest1 = [listeBUser,listeMUser,listeSUser]

    let listeBComp = ["03","02","01","00"]
    let listeMComp  = ["03","02","01","00"]
    let listeSComp  = ["03","02","01","00"]
    let listeMovRest2 = [listeBComp,listeMComp,listeSComp]

let new1 = formaterListeMov listeMovRest1
let new2 = formaterListeMov listeMovRest2
