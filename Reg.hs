{-#LANGUAGE FlexibleContexts, OverloadedStrings #-}
import Text.Regex.TDFA
import Data.Array
import Control.Monad
import Control.Monad.Identity
import Text.Parsec
--import Text.Parsec.Char
import Data.String

default(String)

type Replacement a = a -> MatchArray -> a
--data Replacement a = Subgroup { idx :: Int }
--                   | Part { prt :: String }
--                   deriving (Show)

constR :: (IsString a, Extract a, Stream s m Char) => String -> ParsecT s u m (Replacement a)
constR s = return $ \ _ _ -> (fromString s)
--constR s = return $ Part s

subgroupR :: (IsString a, Extract a, Stream s m Char) => Int -> ParsecT s u m (Replacement a)
subgroupR i= return $ \s m -> extract (m ! i) s
--subgroupR i = return $ Subgroup i

part :: (IsString a, Extract a, Stream s m Char) => ParsecT s u m (Replacement a)
part = many1 (noneOf "$") >>= constR 

mgroup :: (IsString a, Extract a, Stream s m Char) => ParsecT s u m (Replacement a)
mgroup = try (char '$' >> fmap read (many1 digit) >>= subgroupR)

dollar :: (IsString a, Extract a, Stream s m Char) => ParsecT s u m (Replacement a)
dollar = char '$' >> char '$' >> constR "$"

replacement :: (IsString a, Extract a, Stream s m Char) => ParsecT s u m [Replacement a]
replacement = many (part <|> mgroup <|> dollar)


--replace :: (IsString a, Monoid a, Extract a, Stream s Identity Char) => a -> 
replace :: (IsString a, Monoid a, Extract a, RegexOptions r c e, RegexLike r a, RegexMaker r c e sr, Stream s Identity Char) => a -> sr -> s -> a
replace a sr s = doreplace a (matchAll (makeRegex sr) a) s

doreplace :: (IsString a, Monoid a, Extract a, Stream s Identity Char) => a -> [MatchArray] -> s -> a 
doreplace a ms s = prc ms
   where prc []  = a
         prc ms' = mconcat $ repl $ finl $ extr $ foldl twist ([],0) ms'
         twist (as,sm) m = ((sm,(offs m)-sm):as,(offs m)+(len m))
         offs m  = fst (m ! 0)
         len  m  = snd (m ! 0)
         extr (is,l) = (map (flip extract $ a) is, l)
         finl (ex,l) = (after l a):ex
         repl  []    = []
         repl  (p:ps)= case (runParser replacement () "Replacement String" s) of
                        Right rs  -> (snd $ foldl (dorep rs) (reverse ms,[]) ps) ++ [p]
                        Left  err -> []
         dorep rs (m:ms',ss) p = (ms',p:(mconcat $ map (\f -> f a m) rs):ss)

excise :: (Monoid s, Extract s) => s -> [MatchArray] -> s 
excise s ms = prc ms
   where prc []  = s
         prc ms' = mconcat $ reverse $ finl $ extr $ foldl twist ([],0) ms'
         twist (as,sm) m = ((sm,(offs m)-sm):as,(offs m)+(len m))
         offs m = fst (m ! 0)
         len  m = snd (m ! 0)
         extr (is,l) = (map (flip extract $ s) is, l)
         finl (ex,l) = (after l s):ex

mat :: String -> String -> [MatchArray]
mat r s = matchAll (mak r) s

str::String
str =  "ich schau dann mal was ich machen kann, schau du nur das du nicht wegschaust"
k :: [MatchArray]
k= mat "schau" str

mts :: IO ()
mts = do
   forM_ (map (\a -> a ! 0) k) (\a -> do
      putStrLn $ show a)
   return ()

twist :: ([(Int,Int)],Int) -> MatchArray -> ([(Int,Int)],Int)
twist (t,s) m = ( (a-s,s) : t, a + b )
   where a= fst (m ! 0)
         b= snd (m ! 0)

finl  :: Int -> ([(Int,Int)],Int) -> [(Int,Int)]
finl l (t, s)= reverse $ (l-s,s):t


mak :: RegexMaker Regex CompOption ExecOption String => String -> Regex
mak = makeRegex 

takeOut :: String -> String -> [String]
takeOut r s = trf mts
   where mts :: [MatchArray] 
         mts = match (mak r) s
         trf [] = [s]
         trf a  = map (\(t,d)-> take t (drop d s)) $ finl (length s) $ foldl twist ([],0) a

--r :: (RegexLike Regex String, RegexMaker Regex CompOption ExecOption String) => Regex
r :: Regex
r = makeRegex ("[[:space:]]+" :: String) 

str2 :: String
str2 = "   es hallo aaa=aaa koennen halllo >100< oder feg=feg >2000< leute hallllllo sein, nok=bok oder halo nur >1< hallo   "

m1 :: [MatchArray]
m1 = match r str2
