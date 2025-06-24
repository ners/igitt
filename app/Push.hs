module Push where

import Dirty
import Params
import WorkingBranch
import Prelude

{-
 - igitt push ->
 -     "I have made some fixes that I want to (re-)push to N and stay on N."
 -     if you are currently on <name>-<N>, it
 -          OFFERS to create new / amend existing commit
 -              if new, offers the message line
 -          fetches and rebases to upstream master
 -          pushes to <name>-<N>
 -          pushes to <name>-pr
 -     otherwise, fails, you should use igitt new
 -}
newtype PushParams m = PushParams
    { dirtyAction :: m (DirtyAction m)
    }

parseParams
    :: forall m
     . (forall a. Parser a -> Parser (m a))
    -> Parser (PushParams m)
parseParams f = do
    dirtyAction <- f $ parseDirtyAction f
    pure PushParams{..}

askParams :: PushParams Maybe -> PushParams IO
askParams PushParams{..} =
    PushParams
        { dirtyAction = askDirtyAction dirtyAction
        }

push :: Params Identity -> PushParams Maybe -> IO WorkingBranch
push Params{..} (askParams -> PushParams{..}) = do
    whenM isDirty $
        dirtyAction >>= \case
            Amend{..} -> do
                msgArgs <-
                    amendMessage <&> \case
                        KeepOldMessage -> ["--no-edit"]
                        NewCommitMessage msg -> ["--message", msg]
                run_ $ ["git", "commit", "--all", "--amend"] <> msgArgs
            NewCommit{..} -> do
                msg <- commitMessage
                run_ ["git", "commit", "--all", "--message", msg]
    currBranch <- getCurrWorkingBranch
    run_
        ["git", "pull", "--rebase", runIdentity sourceRemote, runIdentity mainBranch]
    run_
        [ "git"
        , "push"
        , "--force"
        , "--set-upstream"
        , runIdentity targetRemote
        , showBranch currBranch
        ]
    run_
        [ "git"
        , "push"
        , "--force"
        , runIdentity targetRemote
        , showBranch currBranch <> ":" <> showPrBranch currBranch
        ]
    pure currBranch
