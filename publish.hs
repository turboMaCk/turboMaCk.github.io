{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

import System.Directory
import System.Environment
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)


-- Settings


directories :: [FilePath]
directories = ["css", "js", "posts", "tags", "assets"]

files :: [FilePath]
files = ["index.html", "archive.html", "rss.xml", "favicon.png", "nonsense"]

distributionLocation :: FilePath
distributionLocation = "_site"


-- Utils


getAbsolutePath :: FilePath -> FilePath -> FilePath
getAbsolutePath root = (++) $ root ++ "/"

-- mappendIf :: forall (m :: * -> *). Monad m => m Bool -> m () -> m ()
mappendIf :: IO Bool -> IO () -> IO ()
mappendIf check action =
    check >>= (\ b -> when b $ action)

removeDirIfExists :: FilePath -> IO ()
removeDirIfExists path =
    doesDirectoryExist path `mappendIf` removeDirectoryRecursive path

moveDirIfExists :: FilePath -> FilePath -> IO ()
moveDirIfExists source target =
    doesDirectoryExist source `mappendIf` renameDirectory source target

moveFileIfExists :: FilePath -> FilePath -> IO ()
moveFileIfExists source target =
    doesFileExist source `mappendIf` renameFile source target


-- Main actions


moveFiles :: FilePath -> FilePath -> IO ()
moveFiles source target =
    let source_ d =
            getAbsolutePath source d
        target_ d =
            getAbsolutePath target d
        moveDirIfExists_ d =
            moveDirIfExists (source_ d) (target_ d)
        moveFileIfExists_ d =
            moveFileIfExists (source_ d) (target_ d)

    in
        mapM_ moveDirIfExists_ directories `mappend`
        mapM_ moveFileIfExists_ files

perform :: FilePath -> IO ()
perform currentDir =
    mapM_ removeDirIfExists directories `mappend`
    moveFiles distributionLocation currentDir

main :: IO ()
main = do
    args <- getArgs
    let notInteractive = elem "-y" args || elem "--yes" args
    let isYes answer = answer == "yes" || answer == "y"

    currentDir <- getCurrentDirectory

    case notInteractive of
        True -> do
            perform currentDir

        False -> do
            putStrLn $ "Current directory is " ++ currentDir ++ "."
            putStrLn "Please confirm that you want to perform actions in this directory to continue."
            putStrLn "Answer yes or no."
            answer <- getLine

            if isYes answer
            then perform currentDir
            else hPutStrLn stderr "User interrupt."
