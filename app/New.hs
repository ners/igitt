module New where

import Options.Applicative
import Params (Params (..))
import Params qualified
import WorkingBranch
import Prelude

{-
 - igitt new <name>
 -     if any <name>-<N> already exists, it FAILS
 -     otherwise,
 -         it creates <name>-1 from upstream/master
 -         creates an empty commit (asks the user for a message)
 -}

data NewParams m = NewParams
    { branchName :: m Text
    , commitMessage :: m Text
    }

parseParams
    :: forall m
     . (forall a. Parser a -> Parser (m a))
    -> Parser (NewParams m)
parseParams f = do
    branchName <-
        f $
            strArgument
                ( metavar "BRANCH"
                  <> help "The base name of the branch to create"
                )
    commitMessage <-
        f $
            strOption
                ( long "message"
                    <> short 'm'
                    <> metavar "MESSAGE"
                    <> help "The commit message for the initial commit"
                )
    pure NewParams{..}

askParams :: NewParams Maybe -> NewParams IO
askParams NewParams{..} =
    NewParams
        { branchName = maybe (textInput "Branch name:" "") pure branchName
        , commitMessage =
            maybe (multilineTextInput "Commit message:" "") pure commitMessage
        }

new :: Params Maybe -> NewParams Maybe -> IO ()
new (Params.defaults -> Params{..}) (askParams -> NewParams{..}) = do
    -- TODO: check if <branchName>-<N> exists for any N
    base <- branchName
    let wb = WorkingBranch{base, n = 1}
    run_ ["git", "fetch", runIdentity sourceRemote, runIdentity mainBranch]
    run_
        [ "git"
        , "checkout"
        , "-b"
        , showBranch wb
        , runIdentity sourceRemote <> "/" <> runIdentity mainBranch
        ]
    msg <- commitMessage
    run_
        ["git", "commit", "--allow-empty", "--allow-empty-message", "--message", msg]
