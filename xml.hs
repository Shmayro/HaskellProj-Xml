module Main where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Text.Parsec hiding ((<|>), many,try)
import Control.Applicative

--Projet
type XmlAttribut = (String,String)
type XmlNom = String
type XmlText = String
data XmlElement a= MkXmlElement (XmlNom,[XmlAttribut],a) deriving (Show,Read)
data XmlTree  = MkXmlText  XmlText  | MkXmlTag (XmlElement [XmlTree]) deriving (Show,Read)


first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

showXmlText :: XmlText -> String
showXmlText x = x

showXmlAttribut :: XmlAttribut -> String
showXmlAttribut xs= fst xs ++ "=\"" ++snd xs ++"\""

showXmlNom :: XmlNom -> String
showXmlNom x = x


showXmlElement :: (a -> String) -> XmlElement a -> String
showXmlElement howtoshow el = case el of
        MkXmlElement (n,attrs,trou) -> "<"++ showXmlNom n
            ++concat [ (" "++(showXmlAttribut attr)) |attr<-attrs] ++">"
            ++howtoshow trou
            ++"</"++showXmlNom n++">"

showXmlTree :: XmlTree -> String
showXmlTree (MkXmlText text) = showXmlText text
showXmlTree (MkXmlTag el) = showXmlElement f el
    where
            f :: [XmlTree] -> String
            f fils = concat (map showXmlTree fils)


--ReadXmlAttribut
parserXmlAttribut=do
             name <- (many letter)
             spaces
             char '='
             spaces
             char '\"'
             value <- (many $ noneOf "\"")
             char '\"'
             spaces
             return ((name,value))


readXmlAttributWithErrors :: String -> Either ParseError XmlAttribut
readXmlAttributWithErrors text = parse parserXmlAttribut "XmlAttribut" text


readXmlAttribut :: String -> Maybe XmlAttribut
readXmlAttribut text = case (parse parserXmlAttribut "" text) of
                          Right xs -> Just xs
                          otherwises -> Nothing


getXmlAttribut :: (Maybe XmlAttribut) -> XmlAttribut
getXmlAttribut (Just xs)=xs


--name <- (many(noneOf " \\\"`'^></"))
--ReadXmlNom
parserXmlNom=do
             name <- (many letter)
             return name

readXmlNomWithErrors :: String -> Either ParseError XmlNom
readXmlNomWithErrors text = parse parserXmlNom "XmlNom" text

readXmlNom :: String -> Maybe XmlNom
readXmlNom text = case (parse parserXmlNom "" text) of
                          Right xs -> Just xs
                          otherwises -> Nothing

getXmlNom :: (Maybe XmlNom) -> XmlNom
getXmlNom (Just xs)=xs

--ReadXmlNom
parserXmlText = do
                    txt <- anyChar `endBy` (string "</")
                    return txt

readXmlTextWithErrors :: String -> Either ParseError XmlText
readXmlTextWithErrors text = parse parserXmlText "XmlText" text

readXmlText :: String -> Maybe XmlText
readXmlText text = case (parse parserXmlText "" text) of
                          Right xs -> Just xs
                          otherwises -> Nothing

getXmlText :: (Maybe XmlText) -> XmlText
getXmlText (Just xs)=xs



parserMkXmlTag = do
                    spaces
                    char '<'
                    nom <- parserXmlNom
                    spaces
                    attrs <- manyTill (parserXmlAttribut) (try(char '>'))
                    trou <- manyTill (parserXmlTree) (try(string ("</"++nom++">")))
                    spaces
                    return (MkXmlTag (MkXmlElement(nom,attrs,trou)))

parserMkXmlText = do
                    text<-many(noneOf "</")
                    return (MkXmlText text)

parserXmlTree= parserMkXmlTag <|> parserMkXmlText

readXmlTreeWithErrors :: String -> Either ParseError XmlTree
readXmlTreeWithErrors text = parse parserXmlTree "XmlTree" text
readXmlTree text = case (parse parserXmlTree "" text) of
                          Right xs -> Just xs
                          otherwises -> Nothing

getXmlTree :: (Maybe XmlTree) -> XmlTree
getXmlTree (Just xs)=xs


-- Partie XPath


type Condition=(String,String)
type ElementConditions=(String,[Condition],Int)
type XPathQuery=[ElementConditions]

parserCondition= do
                    spaces
                    char '@'
                    nam_attrib <-(many letter)
                    spaces
                    char '='
                    spaces
                    char '\''
                    attrib_Val <-(many $ noneOf "\'")
                    char '\''
                    spaces
                    return (nam_attrib,attrib_Val)

readConditionWithErrors :: String -> Either ParseError Condition
readConditionWithErrors text = parse parserCondition "Condition" text

readCondition text = case (parse parserCondition "" text) of
                          Right xs -> Just xs
                          otherwises -> Nothing

getCondition :: (Maybe Condition) -> Condition
getCondition (Just xs)=xs

parserElementConditions3= do
                            spaces
                            elem<- many letter
                            char '['
                            cdts<- parserCondition `sepBy` (char ',')
                            char ']'
                            char '['
                            cibL<- (many digit)
                            char ']'
                            spaces
                            return (elem,cdts,(read cibL::Int))

parserElementConditions1= do
                            spaces
                            elem<- many letter
                            char '['
                            cdts<- parserCondition `sepBy` (char ',')
                            char ']'
                            spaces
                            return (elem,cdts,(read "0"::Int))
parserElementConditions2= do
                            spaces
                            elem<- many letter
                            spaces
                            return (elem,[],(read "0"::Int))

parserElementConditions= try(parserElementConditions3) <|> try(parserElementConditions1) <|> parserElementConditions2

readElementConditionsWithErrors :: String -> Either ParseError ElementConditions
readElementConditionsWithErrors text = parse parserElementConditions "ElementConditions" text

readElementConditions text = case (parse parserElementConditions "" text) of
                          Right xs -> Just xs
                          otherwises -> Nothing

getElementConditions :: (Maybe ElementConditions) -> ElementConditions
getElementConditions (Just xs)=xs


parserXPathQuery= do
                            spaces
                            char '/'
                            query<-parserElementConditions `sepBy` (char '/')
                            return query

readXPathQueryWithErrors :: String -> Either ParseError XPathQuery
readXPathQueryWithErrors text = parse parserXPathQuery "XPathQuery" text

readXPathQuery text = case (parse parserXPathQuery "" text) of
                          Right xs -> Just xs
                          otherwises -> Nothing

getXPathQuery :: (Maybe XPathQuery) -> XPathQuery
getXPathQuery (Just xs)=xs


--Fonction pour tester l'equivalence des attributs de la requete et des balises 
attrsEq ::[(String,String)]->[(String,String)]->Bool
attrsEq [] _ =True
attrsEq _ [] =True
attrsEq (x:[]) ys= (elem x ys)
attrsEq (x:xs) ys= (elem x ys)&&(attrsEq xs ys)
--elem ("name","hamid") [("name","hamid")]


-- fonction qui cherche dans l'xml
queryonElem :: XPathQuery ->[XmlTree] -> [XmlTree]
queryonElem q xmlel= case q of
    elemC:[] -> case elemC of
        (elem,cndts,i) -> case xmlel of
            ((MkXmlText _):_) -> []
            ((MkXmlTag (MkXmlElement (n,attrs,xs))):[]) ->case (n==elem)&&(attrsEq cndts attrs) of
                                               True -> [(MkXmlTag (MkXmlElement (n,attrs,xs)))]
                                               False -> []
            ((MkXmlTag (MkXmlElement (n,attrs,xs))):xmltags) ->case (n==elem)&&(attrsEq cndts attrs) of
                                               True -> if i>0 then [([(MkXmlTag (MkXmlElement (n,attrs,xs)))]++(queryonElem q xmltags)) !! (i-1)] else ([(MkXmlTag (MkXmlElement (n,attrs,xs)))]++(queryonElem q xmltags))
                                               False -> (queryonElem q xmltags)
    elemC:path -> case elemC of
        (elem,cndts,i) -> case xmlel of
            ((MkXmlText _):_) -> []
            ((MkXmlTag (MkXmlElement (n,attrs,xs))):[]) ->case (n==elem)&&(attrsEq cndts attrs) of
                                               True -> queryonElem path xs
                                               False -> []
            ((MkXmlTag (MkXmlElement (n,attrs,xs))):xmltags) ->case (n==elem)&&(attrsEq cndts attrs) of
                                               True -> if i>0 then [(queryonElem path (xs++(queryonElem q xmltags))) !! (i-1)] else queryonElem path (xs++(queryonElem q xmltags))
                                               False -> queryonElem path (queryonElem q xmltags)

--query :: XPathQuery -> [XmlTree] -> [XmlTree]
--query (q:[]) ((MkXmlTag (MkXmlElement (name,attrs,trou))):_) = concat[queryonElem [q] (MkXmlElement (name,attrs,trou))]
--query (q:_) ((MkXmlTag (MkXmlElement (name,attrs,trou))):[]) = concat[queryonElem [q] (MkXmlElement (name,attrs,trou))]
--query (q:path) ((MkXmlTag (MkXmlElement (name,attrs,trou))):xmltags) = (concat[queryonElem [q] (MkXmlElement (name,attrs,trou))]++(concat [query [q] xmltags]))


--Les Tests

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
