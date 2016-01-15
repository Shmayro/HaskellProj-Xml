				=>Haskell Project<=
					 2015-2016

Haroun El Alami


=> Les Tests
--Les Tests
J'ai Ajouté ces deux ensemble de tests avec l'ensemble des explications des resultats

main :: IO ()
main = do
        print $ "test showText => (showXmlText \"Boooom\")"
        print $ (showXmlText "Boooom")

        print $ "test showAttribut => (showXmlAttribut (\"name\",\"Haroun\"))"
        print $ (showXmlAttribut ("name","Haroun"))

        print $ "test showTree => (showXmlTree( MkXmlTag (MkXmlElement(\"haroun\",[(\"age\",\"22\")],[])) ))"
        print $ (showXmlTree( MkXmlTag (MkXmlElement("haroun",[("age","22")],[])) ))

        print $ "Autre Tests => (showXmlTree( MkXmlTag (MkXmlElement(\"haroun\",[(\"age\",\"22\")],[MkXmlTag (MkXmlElement(\"voiture\",[(\"Marque\",\"BMW\")],[MkXmlTag  (MkXmlElement (\"prix\",[(\"TypeDevise\",\"dollars\")],[MkXmlText \"5000.00\"]) )]))])) ))"
        print $ (showXmlTree( MkXmlTag (MkXmlElement("haroun",[("age","22")],[MkXmlTag (MkXmlElement("voiture",[("Marque","BMW")],[MkXmlTag  (MkXmlElement ("prix",[("TypeDevise","dollars")],[MkXmlText "5000.00"]) )]))])) ))
    
        print $ showXmlTree (MkXmlTag (MkXmlElement ("Livre",[("Titre","Les Miserable"),("Auteur","Victor Hugo")],[])))
        
        print $ "Test Parsing Maybe et Either :"
        print $ "Test Parsing => XmlNom"
        print $ readXmlNom "ffff"

        print $ "Test Parsing => XmlAttribut"
        print $ readXmlAttribut "name=\"haroun\""
        
        print $ readXmlAttributWithErrors "name=\"haroun\""

        
        print $ "test read xml Tree :"
        print $ readXmlTreeWithErrors $ "<div test=\"riiw\" test=\"riiw\" ><divo>gg</divo>ffff<divo>gg</divo><divo>gg</divo></div>"
        print $ showXmlTree $ getXmlTree $ readXmlTree $ "<div test=\"riiw\" test=\"riiw\" >yy<divo>gg</divo>ffff<divo>gg</divo><divo>gg</divo></div>"

        print $ "Test =>"
        print $ readXmlTree $ "\t\n     <p font=\"Rial\" >\n\t<div>Sample Texte</div>   </p>"

        print $ "Test XPath Parsing"
        
        print $ readCondition "@name='haroun'"
        print $ readElementConditions "book"
        print $ readElementConditions "book[@language='french' , @auteur='Victor']"
        print $ readElementConditions "book[@language='french' , @auteur='Victor'][10]"
        print $ readXPathQuery "/book/paper"
        print $ readXPathQuery "/book[@language='french',@auteur='Victor']/paper"

        print $ "Test Xml XPath : affiche tout les personnes"      
        print $ queryonElem [("personne",[],0)] [MkXmlTag(MkXmlElement("personne",[("age","22")],[(MkXmlTag (MkXmlElement("voiture",[("matricule","224-321")],[MkXmlText "Ford Mustang"])))])),MkXmlTag(MkXmlElement("personne",[("age","25")],[(MkXmlTag (MkXmlElement("voiture",[("matricule","22-77")],[MkXmlText "Renolt R4"])))]))]

        print $ "Test Selectionne la premiere personne :"  
        print $ queryonElem [("personne",[],1)] [MkXmlTag(MkXmlElement("personne",[("age","22")],[(MkXmlTag (MkXmlElement("voiture",[("matricule","224-321")],[MkXmlText "Ford Mustang"])))])),MkXmlTag(MkXmlElement("personne",[("age","25")],[(MkXmlTag (MkXmlElement("voiture",[("matricule","22-77")],[MkXmlText "Renolt R4"])))]))]

        print $ "Test Selectionne la deuxieme personne :"  
        print $ queryonElem [("personne",[],2)] [MkXmlTag(MkXmlElement("personne",[("age","22")],[(MkXmlTag (MkXmlElement("voiture",[("matricule","224-321")],[MkXmlText "Ford Mustang"])))])),MkXmlTag(MkXmlElement("personne",[("age","25")],[(MkXmlTag (MkXmlElement("voiture",[("matricule","22-77")],[MkXmlText "Renolt R4"])))]))]

        print $ "Afficher la voiture avec la matriculation 224-321 (utilisant la le parsing de la requete)"
        print $ queryonElem (getXPathQuery (readXPathQuery "/personne/voiture[@matricule=\"224-321\"]")) [MkXmlTag(MkXmlElement("personne",[("age","22")],[(MkXmlTag (MkXmlElement("voiture",[("matricule","224-321")],[MkXmlText "Ford Mustang"])))])),MkXmlTag(MkXmlElement("personne",[("age","25")],[(MkXmlTag (MkXmlElement("voiture",[("matricule","22-77")],[MkXmlText "Renolt R4"])))]))]

        print $ "test balise bimo avec attribut nom=Haroun"
        print $ queryonElem [("bimo",[("age","22")],0)] [MkXmlTag(MkXmlElement("bimo",[("age","22")],[(MkXmlTag (MkXmlElement("tko",[("marque","vita")],[MkXmlText "txtbalise1"])))])),MkXmlTag(MkXmlElement("bimo",[("age","22")],[(MkXmlTag (MkXmlElement("tko",[("age","22")],[MkXmlText "TextBalise2"])))]))]

        print $ "Test de la fonction qui permet de parser l'egalite des attribut et attribut de requète"
        print $ attrsEq [("1","3"),("2","2")] [("2","2"),("1","2")]
        --t parseAttribtest
        testxml


testxml = do 
            x <- readFile "test.xml"
            print $ "Recupère les balise ayant comme nom de balise 'body'"
            print $ queryonElem (getXPathQuery (readXPathQuery "/body")) [getXmlTree (readXmlTree x)]

            print $ "Affiche l'ensemble des auteurs"
            print $ concat $ map showXmlTree $ queryonElem (getXPathQuery (readXPathQuery "/body/info/authors")) [getXmlTree (readXmlTree x)]

            print $ "Affiche la premiere range des infos"
            print $ concat $ map showXmlTree $ queryonElem (getXPathQuery (readXPathQuery "/body/info/authors[][1]")) [getXmlTree (readXmlTree x)]



=> Source d'inspiration

au debut je me suis trouvé perdu , car j'avais pas bien compris les methodes utilisés dans le TP4

Donc j'ai commencé par faire des recherches sur le parsing.

Après avoir bien assimiler le fonctionnement du parsing, J'ai commencé le travail sur le projet

Parmis les liens qui m'ont bien aider à comprendre, je cite :

	Video conference  : Parsing Stuff in Haskell  
	https://www.youtube.com/watch?v=r_Enynu_TV0

	An introduction to parsing text in Haskell with Parsec :
	http://unbui.lt/#!/post/haskell-parsec-basics

	les functions possible : 
	https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Combinator.html

	quelque exemples : 
	http://book.realworldhaskell.org/read/using-parsec.html


