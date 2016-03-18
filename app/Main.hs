{-#LANGUAGE Arrows, PackageImports, NoMonomorphismRestriction, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RelaxedPolyRec #-}
{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules#-}
module Main where
import Control.Arrow.ArrowIO
import Control.Monad hiding (when)
import Control.Monad.State hiding (when)
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Text.XML.HXT.DOM.QualifiedName
import Text.XML.HXT.DOM.XmlKeywords (a_source) --for Attribute a_source  - sourcefile read from
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Text.ParserCombinators.Parsec
import Data.Tree.NTree.TypeDefs
import Data.AssocList
import Data.List
import Text.Printf
import System.Environment
import System.FilePath
import System.Directory
import Lib
import Shelly hiding (when,FilePath,(</>),put,get)
import qualified Data.Text as T

default(Int,Double,T.Text)

data Footnote = Footnote {
   ftid      :: Int,
   ftsection :: Int,
   ftcontent :: [XmlTree],
   ftnum     :: String
} deriving (Show)

data Pagebreak = Pagebreak {
   pgid       :: String,
   pgn        :: String,
   pgsection  :: Int
} deriving (Show)

data EpubMeta = EpubMeta {
   idcnt       :: Int,
   seccnt      :: Int,
   toccnt      :: Int,
   marks       :: [(String,Int)],
   footnotes   :: [Footnote],
   pagebreaks  :: [Pagebreak],
   outputfiles :: [FilePath],
   sourcefiles :: SourceFiles
} deriving (Show)

data SourceFiles = SourceFiles {
   sourcePath :: FilePath,
   targetPath :: FilePath,
   tidyPath   :: FilePath,
   filenames  :: [String]
} deriving (Show,Read)

type EpubArrow b c = IOStateArrow EpubMeta b c
type Epub = StateT EpubMeta IO

inputOpts :: SysConfigList
inputOpts = [withValidate no, withParseHTML yes]

outputOpts :: SysConfigList
outputOpts = [withIndent yes, withOutputXHTML, withAddDefaultDTD yes] 

main :: IO ()
main = getArgs >>= bkv

bkv :: [String] -> IO ()
bkv []    = putStrLn "Usage: bkv.exe <epub-source-definition-file>.def"
bkv (s:_) = readFile s >>= readIO >>= (return . epubInit) >>= runStateT createEpub >> return ()

epubInit :: SourceFiles -> EpubMeta
epubInit = EpubMeta 1 1 0 [] [] [] [] 

createEpub :: Epub ()
createEpub = checkTargetDir >> sections >>= mapM_ processSection >> processFeatures >> tidy

checkTargetDir :: Epub ()
checkTargetDir = fmap (targetPath.sourcefiles) get >>= liftIO.(createDirectoryIfMissing True)

sections :: Epub [[FilePath]]
sections = do
   sf <- fmap sourcefiles $ get
   let sp= sourcePath sf
       fl= map (\fn -> (fn, sp ++ fn)) $ filenames sf
   forM fl (\(fn,fp) -> filesFor fp >>= return . ((:) fp) )

tidy :: Epub ()
tidy = do
   e <- get
   let o= filter (isSuffixOf ".xhtml").outputfiles $ e
       t= T.pack $ tidyPath $ sourcefiles e
   forM_ o (\fp -> do
      liftIO $ putStrLn $ "Tidy : " ++ fp
      shelly $ errExit False $ run "tidy" [ "-m", "-config", t, T.pack fp ])
   

filesFor :: FilePath -> Epub [FilePath]
filesFor fp= lift $ loop [] (takeBaseName fp) (takeDirectory fp) [1..] 
   where loop fps b d (n:ns)= do
            let c= d </> (b ++ "-" ++ (show n) ++ ".html")
            x <- doesFileExist c
            if x then
               loop fps b d ns >>= return . ((:) c)
            else
               return []
      
runEpubA :: IOStateArrow EpubMeta XmlTree c -> Epub [c]
runEpubA f = do
   e <- get
   (x, res) <- lift $ runIOSLA (root [] [] >>> f) (initialState e) undefined
   put $ xioUserState x
   return res

processSection :: [FilePath] -> Epub ()
processSection a@(f:_)= do
   h <- runEpubA (readDocument inputOpts (toUri f) >>> hExtract)
   n <- runEpubA (readChapter a >>> scExtract)
   runEpubA (constA (createDoc (h++n)) >>> removeDocWhiteSpace >>> canonicalizeAllNodes >>> writeSection)
   return ()

processFeatures :: Epub ()
processFeatures = do
   ns <- runEpubA fnOutput'
   runEpubA ((constA $ createDoc ns) &&& (constA "Notes.xhtml") >>> writeDoc)
   ps <- runEpubA pgOutput
   runEpubA ((root [] [selem "pagelist" (map constA ps)]) &&& (constA "Pages.xml") >>> writeDoc )
   return ()
   
readChapter :: [String] -> EpubArrow b XmlTree
readChapter fls= constL fls >>> arr toUri >>> readFromDocument inputOpts >>> proc x -> do
   v <- getAttrValue a_source -< x
   arrIO(\s -> putStrLn $ "Read file " ++ s) -< v
   returnA -< x

toUri :: FilePath -> String
toUri = (++) "file://"

writeDoc :: EpubArrow (XmlTree,String) XmlTree
writeDoc = proc (x,n) -> do
   e <- getUserState -< ()
   let tp= (targetPath.sourcefiles $ e ) </> n
   setUserState -< e { outputfiles = tp : (outputfiles e) } 
   arrIO(\s -> putStrLn $ "Write " ++ s) -< tp
   y <- app <<< first (arr $ writeDocument outputOpts) -< (tp,x)
   returnA -< y

writeSection :: EpubArrow XmlTree XmlTree
writeSection = 
   this &&& getUserState >>> second (arr  inc >>> setUserState >>> arr fp) >>> writeDoc
   --getUserState &&& this >>> first (arr  inc >>> setUserState >>> arr wrt) >>> app
   where --wrt e = writeDocument outputOpts (secFile ((seccnt e)-1))
         fp  e = secFile ((seccnt e)-1)
         inc e = e {seccnt = 1 + (seccnt e)}

createDoc :: [XmlTree] -> XmlTree
createDoc cs = head $ runLA doc ()
   where doc = root [] [mkelem "html" [sattr "xmlns:epub" "http://www.idpf.org/2007/ops"] [selem "body" (map constA cs) ]]

hExtract :: EpubArrow XmlTree XmlTree
hExtract = deep (scTitle) >>> arr(\txt -> XN.mkElement (mkName "h1") [] [XN.mkText txt])
   where scTitle = hasName "span" >>> hasAttrValue "class" ((==) "a2textg") >>> countToc >>> getChildren >>> isText >>> getText

scExtract :: EpubArrow XmlTree XmlTree
scExtract = deep (isContent `guards` prCollect) >>> countToc >>> getChildren
   where isContent = hasName "td" /> hasName "h1" 

prCollect :: EpubArrow XmlTree XmlTree
prCollect = processChildren (porh1 >>> pcfn >>> pgCollect >>> fncmpl >>> chgh2)

   where porh1  = filterA (hasName "p" <+> hasName "h1")
         
         pcfn   = processChildren (fnCollect `when` (hasName "a" >>> hasAttrValue "class" ((==) "a2textf"))) `when` hasName "p"

         fncmpl = ifA isFnPar fnComplete this 

         chgh2  = setElemName (mkName "h2") `when` hasName "h1"

         isFnPar= hasName "p" /> hasName "a" >>> hasAttrValue "class" ((==) "a2text") 

countToc :: EpubArrow XmlTree XmlTree
countToc = proc x -> do
   e <- getUserState -< x
   setUserState -< e { toccnt = 1 + (toccnt e) }
   returnA -< x

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

pgOutput :: EpubArrow b XmlTree
pgOutput = proc x -> do
   e <- getUserState -< x
   let st= 1 + (toccnt e)
   ns <- arr (uncurry pgXml) <<< unlistA -< (zip (reverse $ pagebreaks e) [st..])
   returnA -< ns

-- <pagelist>
--  <pagetarget id="p275" type="normal" value="275" playorder="558">
--    <navlabel>
--      <text>
--        275
--      </text>
--    </navlabel>
--    <content src="Section-0001.xhtml#pg275"></content>
--  </pagetarget>
pgXml :: Pagebreak -> Int ->  XmlTree
pgXml (Pagebreak i n s) c = head $ runLA pnode ()
   where pnode = mkelem "pagetarget" [sattr "id" i, sattr "type" "normal", sattr "value" n, sattr "playorder" plord] [lbl, cntnt]
         lbl   = selem "navlabel" [selem "text" [txt n]]
         cntnt = aelem "content" [sattr "src" ((secFile s) ++ "#" ++ i)]
         plord = show c

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
         noteref = const $ "Notes.xhtml#" ++ (noteId i)

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

--fnXml :: [XmlTree] -> [Footnote] -> [XmlTree]
--fnXml tr [] = tr
--fnXml tr (Footnote i s x _ : fs) = fnXml ( pnode : tr ) fs
--   where pnode  = elm "p" [atr "epub:type" "footnote", atr "id" (noteId i)] ([elm "a" [atr "href" ref] isup] ++ x)
--         elm n  = XN.mkElement (mkName n)
--         atr n v= XN.mkAttr (mkName n) [XN.mkText v]
--         ref    = (secFile s) ++ "#" ++ (rnoteId i)
--         isup   = [ elm "i" [] [elm "sup" [] [XN.mkText (show i)]] ]
--         --isup   = xread $ "<i><sup>" ++ (show i) ++ "</sup></i>"

fnXml' :: Footnote -> XmlTree
fnXml' (Footnote i s x _)= head $ runLA pnode ()
   where pnode = mkelem "p" [sattr "epub:type" "footnote", sattr "id" (noteId $ i)] (ahref : map constA x)
         ahref = mkelem "a" [sattr "href" ((secFile s) ++ "#" ++ (rnoteId i))] [ isup ]
         isup  = selem "i" [selem "sup" [txt (show i)]]

--fnOutput :: EpubArrow b ()
--fnOutput = proc x -> do
--   e <- getUserState -< x
--   ns <- arr (fnXml []) -< (footnotes e)
--   writeDocument outputOpts "Notes.xhtml" -< createDoc ns
--   returnA -< ()

--fnXmlA :: (ArrowXml a) => Footnote -> a n XmlTree
--fnXmlA (Footnote i s x _) = pnode 
--   where pnode = mkelem "p" [attr "epub:type" (txt "footnote"), attr "id" (txt . noteId $ i)] (ahref : map constA x)
--         ahref = mkelem "a" [attr "href" (txt $ (secFile s) ++ "#" ++ (rnoteId i))] [ isup ]
--         isup  = selem "i" [selem "sup" [txt (show i)]]
--
--fnXmlA' :: (ArrowXml a) => [Footnote] -> a n XmlTree
--fnXmlA' (Footnote i s x _:fs) = (fnXmlA' fs) <+> pnode
--   where pnode = mkelem "p" [attr "epub:type" (txt "footnote"), attr "id" (txt . noteId $ i)] (ahref : map constA x)
--         ahref = mkelem "a" [attr "href" (txt $ (secFile s) ++ "#" ++ (rnoteId i))] [ isup ]
--         isup  = selem "i" [selem "sup" [txt (show i)]]
--fnXmlA' []      = none


fnOutput' :: EpubArrow b XmlTree
fnOutput' = proc x -> do
   e <- getUserState -< x
   --ns <- app <<< arr (\f -> (catA $ map fnXmlA f,())) -< (footnotes e)
   --ns <- app <<< arr (\f -> (fnXmlA' f,())) -< (footnotes e)
   ns <- arr fnXml' <<< arrL reverse -< (footnotes e)
   returnA -< ns

outputFootnotes :: EpubArrow b ()
outputFootnotes = proc x -> do
   e <- getUserState -< x
   arrIO( \s -> dumpnotes s) -< e
   returnA -< ()

dumpnotes :: EpubMeta -> IO ()
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

secFile :: Int -> String
secFile = printf "Section-%04d.xhtml"


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

