{-#LANGUAGE Arrows, PackageImports, NoMonomorphismRestriction, FlexibleInstances, MultiParamTypeClasses, RelaxedPolyRec #-}
module Main where

import Control.Arrow.ArrowIO
import Control.Monad hiding (when)
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Data.AssocList
import Text.Printf
import Lib

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
   cfile     :: String,
   marks     :: [(String,Int)],
   footnotes :: [Footnote],
   pagebreaks:: [Pagebreak]
}

type EpubArrow b c = IOStateArrow Epubmeta b c

bkvSource :: FilePath
bkvSource = "D:/private/projects/cassian/"

epubInit :: XIOState Epubmeta
epubInit =  initialState $ Epubmeta 0 1 "" [] [] []

main :: IO ()
main = do
   fls <- filesFor "kapitel3050" 
   mapM putStrLn fls 
   --forM fls bkvPar
   bkvPar fls
   return ()

filesFor :: String -> IO [FilePath]
filesFor prefix = return $ takeWhile pureFileExist fls
   where fls = map ((++) bkvSource) $ series prefix


bkvPar :: [FilePath] -> IO ()
bkvPar f = do
   (e,_) <- runEpub (epubInit) (readAll f >>> listA transform )
   runXIOState (initialState $ e) (outputFootnotes)
   return ()

runEpub :: XIOState Epubmeta -> IOStateArrow Epubmeta XmlTree c -> IO (Epubmeta,[c])
runEpub s0 f = do
   (finalState, res) <- runIOSLA (emptyRoot >>> f) s0 undefined
   let usr = xioUserState finalState
   return (usr,res)
   where emptyRoot    = root [] []


readAll :: [String] -> EpubArrow b XmlTree
readAll fls= constL fls >>> arr((++) "file://") >>> (setCurrentFile >>> readFromDocument [withValidate no, withParseHTML yes]) 

transform :: EpubArrow XmlTree XmlTree
transform = proc x -> do
   y <- multi (isContentTd `guards` filterBkvNode) -< x
   str <- writeDocumentToString [withOutputEncoding "iso-8859-1", withIndent yes] -< y
   arrIO (\s -> putStrLn s) -< str
   returnA -< x

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
   forM (footnotes e) $ putStrLn . show
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

addMark :: EpubArrow (String,Int) (String,Int)
addMark = arr id &&& getUserState >>> arr(\(s,e)-> (s, e { marks = s : (marks e) })) >>> second setUserState >>> arr fst

setCurrentFile :: EpubArrow String String
setCurrentFile = getUserState &&& (arr id) >>> arr(\(e,s)-> (e {cfile=s},s)) >>> first setUserState >>> arr snd

getCurrentFile :: EpubArrow b String
getCurrentFile = getUserState >>> arr(cfile)

