-- IO operations for Lamda Control

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE KindSignatures, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}

module LCIO where

import Prelude hiding (log)

import Data.String.Utils as S

import System.Directory

lcDir :: FilePath
lcDir = ".lc"

objectsDir :: FilePath
objectsDir = join "/" [lcDir, "objects"]

headFile :: FilePath
headFile = join "/" [lcDir, "head"]

stagingFile :: FilePath
stagingFile = join "/" [lcDir, "staging"]

-- | Converts the specified SHA-1 hash into a tuple of (directory, file name) 
-- where the directory is the first 2 digits of the SHA and the file name is 
-- composed of the remaining digits.
toFilePath :: String -> IO FilePath
toFilePath sha = 
    case sha of 
        (c1:c2:n) -> 
            let dir = [c1, c2]
                path = S.join "/" [objectsDir, dir] 
            in
            do 
                _ <- createDirectoryIfMissing True path
                return (S.join "/" [path, n])
        _ -> undefined

