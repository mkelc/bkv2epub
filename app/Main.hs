{-#LANGUAGE Arrows, PackageImports, NoMonomorphismRestriction, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RelaxedPolyRec #-}
module Main where
--TODO erste Datei via Top-Arrow einlesen, die Folgenden dateien mit RunX in einer IO Monade lesen und diese via ArrIO
--     innerhalb der Arrow-Kette ausfuehren!
--TODO im Nachgang Tidy aufrufen?
import Control.Arrow.ArrowIO
import Control.Monad hiding (when)
import Control.Monad.State hiding (when)
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.DOM.QualifiedName
import Text.XML.HXT.DOM.XmlKeywords (a_source) --for Attribute a_source  - sourcefile read from
import Text.ParserCombinators.Parsec
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
} deriving (Show)

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

inputOpts :: SysConfigList
inputOpts = [withValidate no, withParseHTML yes]

outputOpts :: SysConfigList
outputOpts = [withIndent yes, withOutputXHTML, withAddDefaultDTD yes] 

epubInit :: Epubmeta
epubInit =  Epubmeta 0 1  [] [] []


main :: IO ()
main = do
   fls <- filesFor "kapitel3050" 
   mapM putStrLn fls 
   (_,e) <- runStateT (processSection fls) epubInit
   return ()

filesFor :: String -> IO [FilePath]
filesFor prefix = return $ takeWhile pureFileExist fls
   where fls = map ((++) bkvSource) $ series prefix

processSection :: [FilePath] -> Epub ()
processSection fs = do
   n <- runEpubA (readAll fs >>> scExtract)
   runEpubA outputFootnotes
   runEpubA (constA (createSection n) >>> removeDocWhiteSpace >>> canonicalizeAllNodes >>> writeSection )
   return () 

runEpubA :: IOStateArrow Epubmeta XmlTree c -> Epub [c]
runEpubA f = do
   e <- get
   (x, res) <- lift $ runIOSLA (root [] [] >>> f) (initialState e) undefined
   put $ xioUserState x
   return res

writeSection :: EpubArrow XmlTree XmlTree
writeSection = 
   getUserState &&& this >>> first (arr  inc >>> setUserState >>> arr wrt) >>> app
   where wrt e = writeDocument outputOpts (printf "Section-%04d.xhtml" ((seccnt e)-1))
         inc e = e {seccnt = 1 + (seccnt e)}

readAll :: [String] -> EpubArrow b XmlTree
readAll fls= constL fls >>> arr((++) "file://") >>> readFromDocument inputOpts >>> proc x -> do
   v <- getAttrValue a_source -< x
   arrIO(\s -> putStrLn $ "Read file " ++ s) -< v
   returnA -< x

createSection :: [XmlTree] -> XmlTree
createSection cs = XN.mkRoot [] [
   XN.mkElement (mkName "html") [XN.mkAttr (mkName "xmlns:epub") [XN.mkText "http://www.idpf.org/2007/ops"]] [
      XN.mkElement (mkName "body") [] cs ]]

scExtract :: EpubArrow XmlTree XmlTree
scExtract = deep (isContent `guards` prCollect) >>> getChildren
   where isContent = hasName "td" /> hasName "h1" 

prCollect :: EpubArrow XmlTree XmlTree
prCollect = processChildren (porh1 >>> pcfn >>> pgCollect >>> fncmpl >>> chgh2)

   where porh1  = filterA (hasName "p" <+> hasName "h1")
         
         pcfn   = processChildren (fnCollect `when` (hasName "a" >>> hasAttrValue "class" ((==) "a2textf"))) `when` hasName "p"

         fncmpl = ifA isFnPar fnComplete this 

         chgh2  = setElemName (mkName "h2") `when` hasName "h1"

         isFnPar= hasName "p" /> hasName "a" >>> hasAttrValue "class" ((==) "a2text") 

pgCollect :: EpubArrow XmlTree XmlTree
pgCollect = processChildren( pgc `when`(hasName "a" >>> hasAttrValue "class" ((==) "a8text")) )
   where pgc = (\p -> mkelem "span" [sattr "id" (pgId p), sattr "epub:type" "pagebreak", sattr "title" p] [] ) $< pageNum
         pageNum = getChildren >>> isText >>> getText >>> arr(pgParse) >>> pgStore

pgParse :: String -> String
pgParse str = case (parse pgSpec "" str) of
      Left _ -> "" 
      Right s -> s
   where pgSpec = do
          spaces >> char '[' >> spaces >> string "S." >> spaces
          pgn <- (many digit)
          spaces >> char ']'
          return pgn

pgStore :: EpubArrow String String
pgStore = getUserState &&& this >>> arr(\(e,p)-> (pgIns e p,p)) >>> first setUserState >>> arr snd
   where pgIns e p = e { pagebreaks = (Pagebreak (pgId p) p (seccnt e)):(pagebreaks e) }

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
         noteref = const $ "Notes.html#" ++ (noteId i)

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
   forM (reverse $ pagebreaks e) $ putStrLn . show
   return ()

pgId :: String -> String
pgId = (++) "pg"

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

addMark :: EpubArrow (String,Int) (String,Int)
addMark = arr id &&& getUserState >>> arr(\(s,e)-> (s, e { marks = s : (marks e) })) >>> second setUserState >>> arr fst

