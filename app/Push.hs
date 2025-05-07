{-# OPTIONS_GHC -Wno-partial-fields #-}

module Push where

import Data.Text qualified as Text
import Options.Applicative
import Params
import WorkingBranch
import Prelude

parseCommitMessage :: Parser Text
parseCommitMessage =
    strOption
        (long "message" <> short 'm' <> metavar "MESSAGE" <> help "Commit message")

data AmendCommitMessage
    = KeepOldMessage
    | NewCommitMessage Text

parseAmendCommitMessage :: Parser AmendCommitMessage
parseAmendCommitMessage = NewCommitMessage <$> parseCommitMessage

askCommitMessage :: Text -> IO Text
askCommitMessage = multilineTextInput "Commit message:"

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
data DirtyAction m
    = Amend {amendMessage :: m AmendCommitMessage}
    | NewCommit {commitMessage :: m Text}

parseDirtyAction
    :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (DirtyAction m)
parseDirtyAction f = do
    let parseAmend = do
            flag'
                ()
                ( long "amend"
                    <> help "If the workspace is dirty, amend the changes to the previous commit"
                )
            amendMessage <- f parseAmendCommitMessage
            pure Amend{..}
    let parseNewCommit = do
            flag'
                ()
                ( long "commit"
                    <> help "If the workspace is dirty, create a new commit of the changes"
                )
            commitMessage <- f parseCommitMessage
            pure NewCommit{..}
    parseAmend <|> parseNewCommit

askDirtyAction :: Maybe (DirtyAction Maybe) -> IO (DirtyAction IO)
askDirtyAction Nothing = do
    let values =
            [("Amend", 'A'), ("Amend with new message", 'M'), ("New commit", 'C')]
                :: [(Text, Char)]
    selected <- buttons "The workspace is dirty, choose your weapon:" values 0 id
    case selected of
        "Amend" -> pure Amend{amendMessage = pure KeepOldMessage}
        "Amend with new message" -> do
            message <- askCommitMessage . Text.strip =<< run ["git", "log", "-1", "--format=%B"]
            pure Amend{amendMessage = pure $ NewCommitMessage message}
        "New commit" -> do
            message <- askCommitMessage ""
            pure NewCommit{commitMessage = pure message}
        _ -> undefined
askDirtyAction (Just Amend{..}) = pure Amend{amendMessage = maybe (pure KeepOldMessage) pure amendMessage}
askDirtyAction (Just NewCommit{..}) = pure NewCommit{commitMessage = maybe (askCommitMessage "") pure commitMessage}

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

-- | Returns True if the current workspace has unstaged changes to tracked files.
isDirty :: IO Bool
isDirty = do
    numChanges <-
        length . Text.lines
            <$> run ["git", "status", "--untracked-files=no", "--short", "--porcelain=v1"]
    pure $ numChanges > 0

push :: Params Maybe -> PushParams Maybe -> IO WorkingBranch
push (Params.defaults -> Params{..}) (askParams -> PushParams{..}) = do
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
    getCurrBranch >>= \case
        Right currBranch -> do
            run_
                ["git", "pull", "--rebase", runIdentity sourceRemote, runIdentity mainBranch]
            run_ ["git", "push", "--force", runIdentity targetRemote, showBranch currBranch]
            run_
                [ "git"
                , "push"
                , "--force"
                , runIdentity targetRemote
                , showBranch currBranch <> ":" <> showPrBranch currBranch
                ]
            pure currBranch
        Left branchName -> fatalError $ "Expected working branch, got: " <> branchName
