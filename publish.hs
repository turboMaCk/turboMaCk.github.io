import System.Directory
import System.Environment
import System.IO (hPutStrLn, stderr)
import Distribution.Verbosity (normal)

-- This is really only spike of how similar script should work

directories :: [FilePath]
directories = ["css", "js", "posts", "tags"]

files :: [FilePath]
files = ["index.html", "archive.html", "rss.xml", "favicon.png"]

distributionLocation :: FilePath
distributionLocation = "_site"

removeDirIfExists :: FilePath -> IO ()
removeDirIfExists path = do
    exists <- doesDirectoryExist path
    if exists
    then do
        removeDirectoryRecursive path
    else
        return ()

moveDirIfExists :: FilePath -> FilePath -> IO ()
moveDirIfExists source target = do
    -- TODO: do not use do notation
    exists <- doesDirectoryExist source
    if exists
    then do
        renameDirectory source target
    else
        return ()

moveFileIfExists :: FilePath -> FilePath -> IO ()
moveFileIfExists source target = do
    -- TODO: do not use do notation
    exists <- doesFileExist source
    if exists
    then do
        renameFile source target
    else
        return ()

getAbsolutePath :: FilePath -> FilePath -> FilePath
getAbsolutePath root =
  (++) $ root ++ "/"

moveFiles :: FilePath -> FilePath -> IO ()
moveFiles source target = do
    -- This needs serious refactoring
    mapM_ (\ d -> moveDirIfExists (getAbsolutePath source d) (getAbsolutePath target d)) directories
    mapM_ (\ d -> moveFileIfExists (getAbsolutePath source d) (getAbsolutePath target d)) files

perform :: FilePath -> IO ()
perform currentDir = do
    _ <- mapM_ removeDirIfExists directories
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
