module GitFixture (
    withGitRepo,
    addCommit,
    modifyFile,
    createBranch,
    checkoutBranch,
) where

import qualified Data.Text as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TE
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Types.Base (Commit)
import Util.RunCommand (runCommandIn, runCommandIn_)

-- | Creates a fresh, initialized git repository in a temporary directory and runs an action in it.
withGitRepo :: (FilePath -> IO a) -> IO a
withGitRepo action =
    withSystemTempDirectory "excalibur-inttest" $ \dir -> do
        let run = runCommandIn_ dir
        run "git init"
        run "git config user.email 'test@example.com'"
        run "git config user.name 'Test'"
        run "git config commit.gpgsign false"
        action dir

-- | Writes a file and makes a commit adding it, returning the commit hash.
addCommit :: FilePath -> String -> String -> IO Commit
addCommit repo filename content = do
    let run = runCommandIn_ repo
    -- Create and add the file
    writeFile (repo </> filename) content
    run $ "git add " ++ filename
    run $ "git commit -m 'add " ++ filename ++ "'"
    -- Read back the commit hash
    (_, out) <- runCommandIn repo "git rev-parse HEAD"
    return (T.pack . TL.unpack . TL.strip . TE.decodeUtf8 $ out)

-- | Modifies an existing file and commits the change, returning the commit hash.
modifyFile :: FilePath -> String -> String -> IO Commit
modifyFile repo filename content = do
    let run = runCommandIn_ repo
    writeFile (repo </> filename) content
    run $ "git add " ++ filename
    run $ "git commit -m 'modify " ++ filename ++ "'"
    (_, out) <- runCommandIn repo "git rev-parse HEAD"
    return (T.pack . TL.unpack . TL.strip . TE.decodeUtf8 $ out)

-- | Creates a new branch at the current HEAD.
createBranch :: FilePath -> String -> IO ()
createBranch repo name = runCommandIn_ repo $ "git branch " ++ name

-- | Checks out an existing branch.
checkoutBranch :: FilePath -> String -> IO ()
checkoutBranch repo name = runCommandIn_ repo $ "git checkout " ++ name
