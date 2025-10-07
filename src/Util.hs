module Util (
    checkoutCommit,
    inTempCopy,
) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process.Typed

import Types

copyTree :: FilePath -> FilePath -> IO ()
copyTree src dst = do
    isDir <- doesDirectoryExist src
    if isDir
        then do
            createDirectoryIfMissing True dst
            entries <- listDirectory src
            forM_ entries $ \name -> do
                let s = src </> name
                    d = dst </> name
                isSubDir <- doesDirectoryExist s
                isFile <- doesFileExist s
                if isSubDir
                    then copyTree s d
                    else when isFile (copyFile s d)
        else do
            isFile <- doesFileExist src
            when isFile $ do
                createDirectoryIfMissing True (takeDirectory dst)
                copyFile src dst

inTempCopy :: (MonadIO m, MonadMask m) => FilePath -> String -> (FilePath -> m a) -> m a
inTempCopy dir prefix action = do
    cwd <- liftIO getCurrentDirectory
    withTempDirectory cwd prefix $ \tmpDir -> do
        let tmpName = takeFileName tmpDir
        entries <- liftIO $ listDirectory dir
        let toCopy = filter (/= tmpName) entries
        forM_ toCopy $ \name -> do
            let src = dir </> name
                dst = tmpDir </> name
            isDir <- liftIO $ doesDirectoryExist src
            if isDir
                then liftIO $ copyTree src dst
                else liftIO $ copyFile src dst

        action tmpDir

checkoutCommit :: FilePath -> Commit -> IO ()
checkoutCommit repo hash = do
    _ <- readProcess_ $ setWorkingDir repo $ shell ("git checkout " ++ T.unpack hash)
    return ()
