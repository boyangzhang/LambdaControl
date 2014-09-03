{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE KindSignatures, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, DataKinds #-}

module Object where

import LCIO

import Codec.Compression.Zlib

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.List as List
import Data.List.Split
import Data.Digest.Pure.SHA
import Data.String.Utils

import System.Directory


fileMode :: String
fileMode = "100644"

dirMode :: String
dirMode = "004000"

data Object = Blob String 
              | Tree [TreeEntry] 
              | Commit CommitEntry
    deriving (Eq)

data TreeEntry = TreeEntry 
    { obj   :: Object,   -- Blob or Tree
      name  :: String    -- directory or file name
    }
    deriving (Eq)

data CommitEntry = CommitEntry 
    { tree       :: Object, 
      parent     :: String, -- commit entry
      msg        :: String
    }
    deriving (Eq)

instance Show Object where
    show b@(Blob _)    = toString b
    show t@(Tree te)   = (toString t) ++ (foldr (\x -> (++) (show (obj x))) [] te)
    show c@(Commit ce) = (toString c) ++ (toString $ tree ce)

-- | General Object Functions

-- | Takes in an Object and a tokenized filepath. Looks for an object
-- that corresponds to the specified filepath.
getObjectByName :: Object -> [String] -> Maybe Object
getObjectByName o []             = Just o
getObjectByName (Blob _) _       = Nothing
getObjectByName (Tree teL) (x:xs) = 
    case (List.find (\t -> name t == x) teL) of
        Nothing    -> Nothing 
        (Just te ) -> getObjectByName (obj te) xs
getObjectByName (Commit ce) l = getObjectByName (tree ce) l


-- | Generates a SHA1 based on the input string.
generateSHA1 :: String -> Digest SHA1State
generateSHA1 = sha1 . C.pack


-- | Produces a SHA1 for a blob or a tree. Generally follows Git's methodology
-- for hash input, although trees and commits hash slightly differently.
hashObject :: Object -> Digest SHA1State
hashObject = generateSHA1 . toString

getType :: Object -> String
getType (Blob _)   = "blob"
getType (Tree _)   = "tree"
getType (Commit _) = "commit"

getMode :: Object -> String
getMode (Blob _)   = fileMode
getMode (Tree _)   = dirMode
getMode (Commit _) = undefined

-- | Builds a well-formed string for each object type.
toString :: Object -> String
toString (Blob s) = "blob" ++ '\n':s ++ "\n"
-- init removes the last newline character
toString (Tree l) = "tree" ++ "\n" ++ foldl (treeEntToString) "" l
    where 
        -- string formatting is of the form: mode type hash name
        -- example: 100644 blob [hash] foo.txt
        treeEntToString :: String -> TreeEntry -> String
        treeEntToString b te =  
            let o = obj te in
            b ++ (getMode o) ++ 
            ' ':(getType o) ++
            ' ':(showDigest (hashObject o)) ++
            ' ':(name te) ++ "\n"
toString (Commit c) = "commit\n" ++ 
                      "tree " ++ (showDigest (hashObject (tree c))) ++ "\n" ++
                      "parent " ++ (parent c) ++ "\n" ++
                      "message " ++ (msg c) ++ "\n"

-- | Encode/Decode

path :: Object -> IO String
path o = toFilePath (showDigest (hashObject o))


encodeToDisk :: Object -> IO ()
encodeToDisk o = do
    fp <- path o
    case o of
        Blob _ -> C.writeFile fp (encode o)
        Tree te -> do
            encodeTreeEntry te
            C.writeFile fp (encode o)
        Commit ce -> do
            encodeToDisk (tree ce)
            C.writeFile fp (encode o)
    where
        encodeTreeEntry :: [TreeEntry] -> IO ()
        encodeTreeEntry []     = return ()
        encodeTreeEntry (x:xs) = do
            encodeToDisk (obj x)
            encodeTreeEntry xs


-- | Encodes an Object into a ByteString that can be written to disk. Uses 
-- zlib compression. Complement to decode.
encode :: Object -> C.ByteString
encode = compress . C.pack . toString


-- | Should only be used for decoding the staging file. 
decodeStaging :: IO Object
decodeStaging = do
    exists <- doesFileExist stagingFile
    if not exists then return $ Tree []
    else do
        stagingTreeHash <- C.readFile stagingFile
        decodeObject $ C.unpack stagingTreeHash


-- | Takes a string representation of a hash and returns the corresponding 
-- object.
-- TODO: error handle if hash does not exist
decodeObject :: String -> IO Object
decodeObject h = do 
    filePath <- toFilePath h
    cont <- C.readFile filePath
    decode cont


-- | Decodes a ByteString (in compressed format) into an Object. Complement to
-- encode.
decode :: C.ByteString -> IO Object
decode bs = 
    let (x:xs) = lines $ C.unpack $ decompress bs in
        case x of 
            "blob"   -> buildBlob xs
            "tree"   -> buildTree xs (Tree [])
            "commit" -> buildCommit xs
            _        -> undefined -- error?


buildBlob :: [String] -> IO Object
buildBlob l = return $ Blob (join "\n" l)


buildTree :: [String] -> Object -> IO Object
buildTree [] t@(Tree _)   = return t
buildTree (x:xs) (Tree l) = 
    let teArgs = splitOn " " x in 
    do 
        o <- decodeObject $ teArgs !! 2
        let n = teArgs !! 3             -- name
        let te = TreeEntry o n
        -- append to end of list to preserve order of string list
        buildTree xs (Tree (l ++ [te])) 
buildTree _ _            = undefined 


-- | The input list should have 3 items: tree, parent, msg.
buildCommit :: [String] -> IO Object
buildCommit l = do
    treeObj <- decodeObject ((splitOn " " (l !! 0)) !! 1) -- hash of tree
    let parComm = (splitOn " " (l !! 1)) !! 1 -- hash of parent commit
    let m = join " " $ tail (splitOn " " (l !! 2))
    return $ Commit CommitEntry {
                tree = treeObj,
                parent = parComm,
                msg = m
            }


-- | Tree Manipulation

-- | Updates the tree specified as the second argument with the blob specified
-- as the second argument. The array of strings represent the path to the
-- object to be updated.
updateTree :: Object -> Object -> [String] -> Object
updateTree blob (Tree teL) [x] = 
    let newTe = (TreeEntry blob x) in
    case (List.find (\t -> name t == x) teL) of
        -- the blob is a new file, previously untracked
        Nothing -> Tree $ newTe : teL
        -- file was previously tracked, remove old tree entry
        Just curr -> Tree $ newTe : (rmTree curr teL)

updateTree blob (Tree teL) l@(x:xs) = 
    case (List.find (\t -> name t == x) teL) of
        -- new directory and blobs
        Nothing -> Tree $ (mkTreeDir blob l) : teL
        -- current directory already exists
        Just curr -> 
            -- recursive call to build a new object
            let nObj = (updateTree blob (obj curr) xs) in
            -- create a new tree entry with new object 
            let newTe = TreeEntry nObj (name curr) in
            Tree $ newTe : (rmTree curr teL)

updateTree _ (Commit _) _ = undefined -- error?
updateTree _ _ _   = undefined -- error?


-- | Builds up a new tree of directories leading to a blob (first argument).
mkTreeDir :: Object -> [String] -> TreeEntry
mkTreeDir (Blob _) [] =  undefined
mkTreeDir b@(Blob _) [x] = TreeEntry b x
mkTreeDir b@(Blob _) (x:xs) = 
    let treeObj = Tree [(mkTreeDir b xs)] in
    TreeEntry treeObj x
mkTreeDir _ _        = undefined -- TODO error if not blob


-- | Deletes a tree entry from a list by matching on hash.
rmTree :: TreeEntry -> [TreeEntry] -> [TreeEntry]
rmTree _ []     = []
rmTree t (x:xs) =
    let hashT = hashObject (obj t)
        hashX = hashObject (obj x)
    in
    if (showDigest $ hashT) == (showDigest $ hashX) then xs
    else x:(rmTree t xs)
