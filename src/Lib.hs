{-#LANGUAGE Arrows #-}

module Lib
    ( series
    , pureFileExist
    ) where

import System.Directory
import System.IO.Unsafe

series :: String -> [String]
series prefix = map trx [1 ..]
   where trx n = prefix ++ "-" ++ (show n) ++ ".html"

-- maybe ask on stackoverflow for this issue - is there a way 
-- to operate takeWhile in IO context and stay lazy??
pureFileExist :: FilePath -> Bool
pureFileExist = unsafePerformIO . doesFileExist
