import Text.ParserCombinators.Parsec


parsePage :: String -> Int
parsePage str = case (parse pgSpec "" str) of
      Left _ -> 0 
      Right s -> read s
   where pgSpec = do
               spaces >> char '[' >> spaces >> string "S." >> spaces
               pgn <- (many digit)
               spaces >> char ']'
               return pgn
