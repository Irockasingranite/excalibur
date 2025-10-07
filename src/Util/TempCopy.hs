module Util.TempCopy (inTempCopy) where

-- Implements SPEC-4 @relation(SPEC-4, scope=file)

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import System.IO.Temp

-- Recursively copies a directory tree to another location.
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

-- Performs an IO action within a temporary copy of a directory. The copy is removed after the
-- action has finished.
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
