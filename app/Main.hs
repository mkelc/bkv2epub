{-#LANGUAGE Arrows, PackageImports, NoMonomorphismRestriction, FlexibleInstances, MultiParamTypeClasses, RelaxedPolyRec #-}
module Main where
--TODO Pagebreaks einsammeln und die Tags im Text entsprechend bearbeiten
--TODO erste Datei via Top-Arrow einlesen, die Folgenden dateien mit RunX in einer IO Monade lesen und diese via ArrIO
--     innerhalb der Arrow-Kette ausfuehren!
--TODO h1 zu h2, Ueberschrift aus erstem Dokument fuer die Section erstellen
--TODO im Nachgang Tidy aufrufen?
--TODO Namespaces im Header deklarieren
import Control.Arrow.ArrowIO
import Control.Monad hiding (when)
import Control.Monad.State hiding (when)
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.DOM.QualifiedName
import Text.XML.HXT.DOM.XmlKeywords (a_source) --for Attribute a_source  - sourcefile read from
import Data.Tree.NTree.TypeDefs
import Data.AssocList
import Text.Printf
import Lib

import qualified Text.XML.HXT.DOM.XmlNode as XN

data Footnote = Footnote {
   ftid      :: Int,
   ftsection :: Int,
   ftcontent :: [XmlTree],
   ftnum     :: String
   }
   deriving (Show)

data Pagebreak = Pagebreak {
   pgid       :: String,
   pgn        :: String,
   pgsection  :: Int
}

data Epubmeta = Epubmeta {
   idcnt     :: Int,
   seccnt    :: Int,
   marks     :: [(String,Int)],
   footnotes :: [Footnote],
   pagebreaks:: [Pagebreak]
}

type EpubArrow b c = IOStateArrow Epubmeta b c
type Epub = StateT Epubmeta IO

bkvSource :: FilePath
bkvSource = "D:/private/projects/cassian/"

epubInit :: Epubmeta
epubInit =  Epubmeta 0 1  [] [] []


main :: IO ()
main = do
   fls <- filesFor "kapitel3050" 
   mapM putStrLn fls 
   (_,e) <- runStateT (eBkvPar fls) epubInit
   return ()

filesFor :: String -> IO [FilePath]
filesFor prefix = return $ takeWhile pureFileExist fls
   where fls = map ((++) bkvSource) $ series prefix

eBkvPar :: [FilePath] -> Epub ()
eBkvPar fs = do
   n <- runEpubA (readAll fs >>> scExtract)
   runEpubA outputFootnotes
   runEpubA (constA (createSection n) >>> writeSection )
   return () 

--runEpub :: Epub a -> Epubmeta -> IO (a, Epubmeta)
--runEpub = runStateT

runEpubA :: IOStateArrow Epubmeta XmlTree c -> Epub [c]
runEpubA f = do
   e <- get
   (x, res) <- lift $ runIOSLA (root [] [] >>> f) (initialState e) undefined
   put $ xioUserState x
   return res

writeSection :: EpubArrow XmlTree XmlTree
writeSection = 
   getUserState &&& this >>> first (arr  inc >>> setUserState >>> arr wrt) >>> app
   where wrt e = writeDocument [withIndent no, withOutputXHTML, withAddDefaultDTD yes] (printf "Section-%04d.xhtml" ((seccnt e)-1))
         inc e = e {seccnt = 1 + (seccnt e)}

readAll :: [String] -> EpubArrow b XmlTree
readAll fls= constL fls >>> arr((++) "file://") >>> readFromDocument [withValidate no, withParseHTML yes] >>> proc x -> do
   v <- getAttrValue a_source -< x
   arrIO(\s -> putStrLn $ "Read file " ++ s) -< v
   returnA -< x

col :: [XmlTree] -> [XmlTree]
col = id

bld :: [[XmlTree]] -> XmlTree
bld xs = NTree (XTag (mkName "bosh") []) (join xs) 

createSection :: [XmlTree] -> XmlTree
createSection cs = XN.mkRoot [] [
   XN.mkElement (mkName "html") [] [
      XN.mkElement (mkName "body") [] cs ]]

scExtract :: EpubArrow XmlTree XmlTree
scExtract = deep (isContent `guards` prCollect) >>> getChildren
   where isContent = hasName "td" /> hasName "h1" 

prCollect :: EpubArrow XmlTree XmlTree
prCollect = processChildren (porh1 >>> pcfn >>> fncmpl >>> chgh2)

   where porh1  = filterA (hasName "p" <+> hasName "h1")
         
         pcfn   = processChildren (fnCollect `when` (hasName "a" >>> hasAttrValue "class" ((==) "a2textf"))) `when` hasName "p"

         fncmpl = ifA isFnPar fnComplete this 

         chgh2  = setElemName (mkName "h2") `when` hasName "h1"

         isFnPar= hasName "p" /> hasName "a" >>> hasAttrValue "class" ((==) "a2text") 

fnCollect :: EpubArrow XmlTree XmlTree
fnCollect = getAttrValue "href" &&& arr id >>> 
   first (arrL idhref &&& nextId) >>>  
   first addMark  >>> 
   first (arr $ fnCreate . snd) >>> app
   
fnCreate :: Int -> EpubArrow XmlTree XmlTree
fnCreate i = replaceChildren (mkelem "sup" [] [mkelem "i" [] [constA (show i) >>> mkText]]) >>> fnLinkAttrs i

fnLinkAttrs :: Int -> EpubArrow XmlTree XmlTree
fnLinkAttrs i = fnAttrs >>> removeAttr "class" >>> addAttr "epub:type" "noteref"
   where fnAttrs = processAttrl $ changeAttrValue (noteref) `when` hasName "href"
         noteref = const $ "Notes.html#" ++ (printf "ft%04d" i)

fnComplete :: EpubArrow XmlTree XmlTree
fnComplete = deep $ getChildren >>> choiceA [isAref :-> fa, isSpan :-> fspan]
   where 
      isAref = hasName "a"
      isSpan = hasName "span"
      fa = proc x -> do
         idn <- getAttrValue "id" -< x
         e <- getUserState -< x
         let v= lookupDef (-1) idn (marks e)
         setUserState -< e { footnotes = (Footnote v (seccnt e) [] idn) : (footnotes e) }
         none -< x
      fspan = proc x -> do
         c <- listA getChildren -< x
         e <- getUserState -< x
         let ftn = head (footnotes e)
         setUserState -< e { footnotes = (ftn {ftcontent = c}):(tail $ footnotes e) }
         none -< x

outputFootnotes :: EpubArrow b ()
outputFootnotes = proc x -> do
   e <- getUserState -< x
   arrIO( \s -> dumpnotes s) -< e
   returnA -< ()

dumpnotes :: Epubmeta -> IO ()
dumpnotes e = do
   forM (reverse $ footnotes e) $ putStrLn . show
   return ()
      
rnoteId :: Int -> String
rnoteId= printf "r%04d"

idhref :: String -> [String]
idhref ('#' : xs) = [xs]
idhref _          = []

nextId :: EpubArrow a Int
nextId = proc x -> do
   e <- getUserState -< x
   let newid= idcnt e
   setUserState -< e {idcnt = newid + 1}
   returnA -< newid

addMark :: EpubArrow (String,Int) (String,Int)
addMark = arr id &&& getUserState >>> arr(\(s,e)-> (s, e { marks = s : (marks e) })) >>> second setUserState >>> arr fst

