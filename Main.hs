-- Advanced Programming
-- by Boyang Zhang (zhangb) and Grace Wang (wangrace)
 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE KindSignatures, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
 
module Main where
import Object
import LCIO

import Prelude hiding (log)

import Control.Monad.IO.Class

import Data.Digest.Pure.SHA
import Data.String.Utils as S
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput

import System.Directory
import System.Environment
import qualified System.IO.Strict as SIO


-- | Checks if VCS is initialized.
checkInit :: IO Bool
checkInit = doesDirectoryExist objectsDir 


-- | Initalizes the repository metadata folders / files. 
lcInit :: IO ()
lcInit = do
    cInit <- checkInit
    pwd <- getCurrentDirectory
    if not cInit then do
        createDirectoryIfMissing True objectsDir
        writeFile headFile "none"
        putStrLn $ "Initialized empty LC repository in " ++ pwd ++ "/.lc"
    else putStrLn $ "Reintialized existing LC repository in " ++ pwd ++ "/.lc"


-- | Takes hash and prints the contents of that object.
catFile :: String -> IO ()
catFile sha = do
    o <- decodeObject sha
    putStrLn $ toString o


-- | Creates encoded file(s) in the .lc/objects directory corresponding to the 
-- specified file.
add :: (MonadIO m) => FilePath -> m ()
add p = do
    -- checks to see if vc is initialized
    cInit <- liftIO checkInit
    -- checks to see if file in path exists
    validFile <- liftIO $ doesFileExist p

    if cInit && validFile then do
        t <- liftIO $ decodeStaging
        fileContent <- liftIO $ readFile p
        -- update tree
        let newTree = updateTree (Blob fileContent) t pathTok
        -- write tree into .lc/objects
        liftIO $ encodeToDisk newTree
        -- update the staging area
        liftIO $ writeFile stagingFile (showDigest (hashObject newTree))
    else if not validFile then
        liftIO $ putStrLn $ "pathspec " ++ p ++ " did not match any files"
    else liftIO $ putStrLn $ "not a LC repository"

    where 
        -- array of strings representing the file path
        pathTok = split "/" p


getHeadCommit :: IO String
-- strict IO monad needed due to lazy evaluation
getHeadCommit = SIO.readFile headFile


commit :: (MonadIO m) => String -> m ()
commit str = do
    staging <- liftIO $ decodeStaging
    headCommit <- liftIO $ getHeadCommit
    case headCommit of
        "none" -> writeCommit staging headCommit
        _      -> do
            Commit ce <- liftIO $ decodeObject headCommit
            -- no change in the staging tree since last commit
            if tree ce == staging
                then liftIO $ putStrLn "nothing to commit, working directory clean"
            else writeCommit staging headCommit

    where
        writeCommit staging headCommit = do 
            let newCommit = Commit $ CommitEntry staging headCommit str
            let commitHash = hashObject newCommit
            -- update head commit
            liftIO $ writeFile headFile (showDigest commitHash)
            -- encode and write new commit object to disk
            liftIO $ encodeToDisk newCommit 


-- | Reverts the repository to the state specified by the given hash.
revert :: (MonadIO m) => String -> m ()
revert h = do
    -- removes all files in staging area
    staging <- liftIO $ decodeStaging
    liftIO $ removeFiles staging
    -- adds all files in given hash
    Commit ce <- liftIO $ decodeObject h
    liftIO $ createFiles (tree ce)

removeFiles :: Object -> IO ()
removeFiles (Tree l) = removeFilesHelper "." l
    where
        removeFilesHelper :: FilePath -> [TreeEntry] -> IO ()
        removeFilesHelper _ [] = return ()
        removeFilesHelper p (x:xs) = do
            rmFile p x
            removeFilesHelper p xs

        -- args: tree entry and the corresponding filepath
        rmFile :: FilePath -> TreeEntry -> IO ()
        rmFile p te = 
            case (obj te) of
                Blob _ -> do
                    let p' = S.join "/" [p, name te]
                    exists <- doesFileExist p' 
                    if exists then removeFile p' else return ()
                Tree l' -> do
                    let dirPath = S.join "/" [p, name te] 
                    removeFilesHelper dirPath l'
                    -- after removing all children, remove dir if empty
                    dirContents <- getDirectoryContents dirPath
                    if length dirContents == 2      -- . and ..
                        then removeDirectory dirPath
                        else return ()
                Commit _ -> undefined
removeFiles _ = undefined

createFiles :: Object -> IO ()
createFiles (Tree l) = createFilesHelper "." l
    where
        createFilesHelper :: FilePath -> [TreeEntry] -> IO ()
        createFilesHelper _ [] = return ()
        createFilesHelper p (x:xs) = do
            createFile p x
            createFilesHelper p xs

        -- args: tree entry and the corresponding filepath
        createFile :: FilePath -> TreeEntry -> IO ()
        createFile p te = 
            case (obj te) of
                Blob s -> writeFile (S.join "/" [p, name te]) s
                Tree l' -> do
                    -- pass False to createDirectoryIfMissing because parent 
                    -- dirs should have been created already
                    let dirPath = S.join "/" [p, name te]
                    createDirectoryIfMissing False dirPath
                    createFilesHelper dirPath l'
                Commit _ -> undefined
createFiles _ = undefined


-- | Takes in two commit hashes and a filename. Shows the difference between 
-- the file contents at the two commits.
diff :: (MonadIO m) => String -> String -> String -> m ()
diff h1 h2 file = do
    Commit c1 <- liftIO $ decodeObject h1 
    Commit c2 <- liftIO $ decodeObject h2
    let pathTok = split "/" file
    let o1 = getObjectByName (tree c1) (pathTok)
    let o2 = getObjectByName (tree c2) (pathTok)
    let tup = (o1, o2)
    case tup of
        (Nothing, Just (Blob s))    -> prettyDiff [] (lines s)
        (Just (Blob s), Nothing)    -> prettyDiff (lines s) []
        (Just (Blob s1), Just (Blob s2)) -> prettyDiff (lines s1) (lines s2)
        (Nothing, Nothing)          -> liftIO $ putStr "Specified filepath does not exist"
        (_, _)                      -> liftIO $ putStr "Specified path is not a file" 
    where 
        prettyDiff l1 l2 = liftIO $ putStr $ ppDiff $ getGroupedDiff l1 l2
 
-- | Finds the head commit on disk and displays information about the last 
-- n commits.
log :: Int -> IO ()
log n = do
    headCommit <- getHeadCommit
    logHelper headCommit n

    where
        logHelper :: String -> Int -> IO ()
        logHelper _ 0       = return ()
        logHelper "none" _  = putStrLn "none"
        logHelper curr k    = do
            Commit ce <- decodeObject curr
            putStrLn $ curr ++ ' ':(msg ce)
            logHelper (parent ce) (k - 1) 


main :: IO ()
main = do
    args <- getArgs
    case args of 
        ["init"]                -> lcInit
        ["add", filename]       -> add filename
        ["catFile", h]          -> catFile h
        ["commit", m]           -> commit m
        ["diff", h1, h2, fn ]   -> diff h1 h2 fn
        ["log"]                 -> log 10   -- use default value of 10
        ["log", n]              -> log (read n)
        ["revert", h]           -> revert h
        []                      -> error "no args"
        _                       -> error "unknown command line arguments"


