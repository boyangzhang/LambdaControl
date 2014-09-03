{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE KindSignatures, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
module Tests where

import LCIO
import Main
import Object

import Control.Monad

import Data.Digest.Pure.SHA
import Data.String.Utils as S

import System.Directory
import System.IO.Unsafe

import Test.HUnit
import Test.QuickCheck hiding (elements)
import Test.QuickCheck.Monadic


-- | Unit Tests

te1 :: TreeEntry
te1 = TreeEntry (Blob "foo contents") "foo"

te2 :: TreeEntry
te2 = TreeEntry (Blob "bar contents") "bar"

te3 :: TreeEntry
te3 = TreeEntry (Blob "baz contents") "baz"

tree :: Object
tree = 
    let tr = Tree [te1, te2]
        te1' = TreeEntry tr "testDir"
        te2' = TreeEntry (Blob "bar contents") "bar"
    in
    Tree [te1', te2']


-- | Clears the .lc folder if it exists and reinitializes the repository so we
-- start at a clean state.
clear :: IO ()
clear = do
    exists <- doesDirectoryExist lcDir
    if exists then removeDirectoryRecursive lcDir else return ()
    lcInit


-- | Returns the number of items in the objects folder that are created as a
-- result of encoding the specified series of objects.
numFilesCreated :: [Object] -> IO Int
numFilesCreated objs = do
    clear
    encodeAll objs
    (_:_:xs) <- getDirectoryContents objectsDir
    return $ foldl (\a b -> a + numFiles (S.join "/" [objectsDir, b])) 0 xs

    where 
        encodeAll :: [Object] -> IO ()
        encodeAll [] = return ()
        encodeAll (o:os) = do
            encodeToDisk o
            encodeAll os

        numFiles :: FilePath -> Int
        numFiles dir = 
           let dirContents = unsafePerformIO $ getDirectoryContents dir in
           length dirContents - 2
            

-- This sequence of tests checks the number of items in the objects directory. 
t0a :: Test
t0a = "t0a" ~: 2 ~=? unsafePerformIO (numFilesCreated [Tree [te1]])

t0b :: Test
t0b = "t0b" ~: 4 ~=? unsafePerformIO (numFilesCreated [Tree [te1], Tree [te1, te2]])

t0c :: Test
t0c = "t0c" ~: 6 ~=? unsafePerformIO (numFilesCreated [Tree [te1], Tree [te1, te2], Tree [te1, te2, te3]])

t0 :: Test
t0 = "t0" ~: TestList [t0a, t0b, t0c]


-- | QuickCheck

-- | Generates a string of at least length 1 from the lowercase letters of the 
-- alphabet.
genString :: Gen String
genString = listOf1 $ choose ('a', 'z')

instance Arbitrary (Digest SHA1State) where
    arbitrary = liftM generateSHA1 arbitrary

instance Arbitrary Object where
    arbitrary = arbKObject 5

instance Arbitrary TreeEntry where
    arbitrary = arbKTreeEntry 5

instance Arbitrary CommitEntry where
    arbitrary = liftM3 CommitEntry arbitrary arbitrary arbitrary 

arbKObject :: Int -> Gen Object
arbKObject k = 

    let blobContents = genString
        -- a tree must contain at least 1 subdirectory or file. Set max to 3 
        -- to avoid stack overflow.
        teL = listOf1 (resize 3 (arbKTreeEntry (k `div` 2)))
    in
    frequency [ (3, liftM Blob blobContents),
                (1, liftM Tree teL) ]

arbKTreeEntry :: Int -> Gen TreeEntry
arbKTreeEntry k = 
    let o = arbKObject (k `div` 2)
        n = genString
    in 
    liftM2 TreeEntry o n

    
-- | Tests whether encoding and decoding an object yields the same object.
prop_identity :: Object -> Property
prop_identity o = 
    let h = hashObject o in
    monadicIO $ do
        run $ encodeToDisk o 
        o' <- run $ decodeObject (showDigest h)
        --run $ putStrLn $ show o
        --run $ putStrLn $ show o'
        Test.QuickCheck.Monadic.assert $ o == o'
