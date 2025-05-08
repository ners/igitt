{-# OPTIONS_GHC -Wno-partial-fields #-}

module Dirty where

import Data.Text qualified as Text
import Options.Applicative (flag', help, long, metavar, short, strOption)
import Prelude

data AmendCommitMessage
    = KeepOldMessage
    | NewCommitMessage Text

parseCommitMessage :: Parser Text
parseCommitMessage =
    strOption
        (long "message" <> short 'm' <> metavar "MESSAGE" <> help "Commit message")

parseAmendCommitMessage :: Parser AmendCommitMessage
parseAmendCommitMessage = NewCommitMessage <$> parseCommitMessage

data DirtyAction m
    = Amend {amendMessage :: m AmendCommitMessage}
    | NewCommit {commitMessage :: m Text}

parseDirtyAction
    :: forall m
     . (forall a. Parser a -> Parser (m a))
    -> Parser (DirtyAction m)
parseDirtyAction f = do
    let parseAmend = do
            flag' () $
                long "amend"
                    <> help "If the workspace is dirty, amend the changes to the previous commit"
            amendMessage <- f parseAmendCommitMessage
            pure Amend{..}
    let parseNewCommit = do
            flag' () $
                long "commit"
                    <> help "If the workspace is dirty, create a new commit of the changes"
            commitMessage <- f parseCommitMessage
            pure NewCommit{..}
    parseAmend <|> parseNewCommit

askDirtyAction :: Maybe (DirtyAction Maybe) -> IO (DirtyAction IO)
askDirtyAction Nothing = do
    let values :: [(Text, Char)]
        values = [("Amend", 'A'), ("Amend with new message", 'M'), ("New commit", 'C')]
    buttons "The workspace is dirty, choose your weapon:" values 0 id >>= \case
        "Amend" -> pure Amend{amendMessage = pure KeepOldMessage}
        "Amend with new message" -> do
            message <-
                commitMessageInput . Text.strip =<< run ["git", "log", "-1", "--format=%B"]
            pure Amend{amendMessage = pure $ NewCommitMessage message}
        "New commit" -> do
            message <- commitMessageInput ""
            pure NewCommit{commitMessage = pure message}
        _ -> undefined
askDirtyAction (Just Amend{..}) = pure Amend{amendMessage = maybe (pure KeepOldMessage) pure amendMessage}
askDirtyAction (Just NewCommit{..}) =
    pure NewCommit{commitMessage = maybe (commitMessageInput "") pure commitMessage}

-- | Returns True if the current workspace has unstaged changes to tracked files.
isDirty :: IO Bool
isDirty =
    (ExitSuccess /=) . fst <$> run' ["git", "diff-index", "--quiet", "HEAD", "--"]
