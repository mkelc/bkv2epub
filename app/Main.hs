{-#LANGUAGE Arrows, PackageImports, NoMonomorphismRestriction, FlexibleInstances, MultiParamTypeClasses, RelaxedPolyRec #-}
module Main where

import Control.Arrow.ArrowIO
import Control.Monad hiding (when)
import Control.Monad.State hiding (when)
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Text.XML.HXT.Arrow.XmlState.TypeDefs
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
   (_,e) <- runEpub (eBkvPar fls) epubInit
   return ()

filesFor :: String -> IO [FilePath]
filesFor prefix = return $ takeWhile pureFileExist fls
   where fls = map ((++) bkvSource) $ series prefix

eBkvPar :: [FilePath] -> Epub ()
eBkvPar fs = do
   n <- runEpubX (readAll fs >>> transform)
   runEpubX outputFootnotes
   runEpubX (constA (createSection n) >>> writeSection )
   return () 

runEpub :: Epub a -> Epubmeta -> IO (a, Epubmeta)
runEpub = runStateT

runEpubX :: IOStateArrow Epubmeta XmlTree c -> Epub [c]
runEpubX f = do
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
readAll fls= constL fls >>> arr((++) "file://") >>> readFromDocument [withValidate no, withParseHTML yes] 

collect :: EpubArrow [XmlTree] XmlTree
collect = proc xs -> do
   arr(\chlds -> NTree (XTag (mkName "html") []) chlds) -< xs

final :: EpubArrow [XmlTree] XmlTree
final = proc xs -> do
   arr(\chlds -> NTree (XTag (mkName "merge") []) chlds) -< xs

createSection :: [XmlTree] -> XmlTree
createSection cs = XN.mkRoot [] [
   XN.mkElement (mkName "html") [] [
      XN.mkElement (mkName "body") [] cs ]]

transform :: EpubArrow XmlTree XmlTree
transform = multi (isContentTd `guards` filterBkvNode) >>> getChildren

isContentTd :: (ArrowXml a) => a XmlTree XmlTree
isContentTd = isElem >>> hasName "td" >>> getChildren >>> isElem >>> hasName "h1"

filterBkvNode :: EpubArrow XmlTree XmlTree
filterBkvNode = processChildren (pcft `when` (hasName "p")) >>>
   processChildren (filterA (hasName "p" <+> hasName "h1")) >>>
   processChildren (ifA isFootnotePar completeFootnote this ) 
   where pcft = processChildren (processFootnotes `when` (hasName "a" >>> hasAttrValue "class" ((==) "a2textf"))) 

processFootnotes :: EpubArrow XmlTree XmlTree
processFootnotes = getAttrValue "href" &&& arr id >>> 
   first (arrL idhref &&& nextId) >>>  
   first addMark  >>> 
   first (arr $ createMark . snd) >>> app
   
createMark :: Int -> EpubArrow XmlTree XmlTree
createMark i = replaceChildren (mkelem "sup" [] [mkelem "i" [] [constA (show i) >>> mkText]]) >>> 
   processAttrl ( changeAttrValue (\_-> createNoteRef i) `when` hasName "href") >>> removeAttr "class"

createNoteRef :: Int -> String
createNoteRef = ((++) "Notes.html#") . noteId

isFootnotePar :: EpubArrow XmlTree XmlTree
isFootnotePar = hasName "p" >>> getChildren >>> hasName "a" >>> hasAttrValue "class" ((==) "a2text")

completeFootnote :: EpubArrow XmlTree XmlTree
completeFootnote = deep $ getChildren >>> choiceA [isAref :-> fa, isSpan :-> fspan]
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
      

noteId :: Int -> String
noteId = printf "ft%04d"

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

nextSection :: Epubmeta -> Epubmeta
nextSection e = e { seccnt= (seccnt e) + 1 }

addMark :: EpubArrow (String,Int) (String,Int)
addMark = arr id &&& getUserState >>> arr(\(s,e)-> (s, e { marks = s : (marks e) })) >>> second setUserState >>> arr fst

