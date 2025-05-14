module Params where

import Control.Monad.Logger (LogLevel (..))
import Data.Char (toLower)
import Data.Text qualified as Text
import System.Environment (lookupEnv)
import Prelude

data Params m = Params
    { verbosity :: m LogLevel
    , sourceRemote :: m Text
    , targetRemote :: m Text
    , mainBranch :: m Text
    }

readLogLevel :: String -> Maybe LogLevel
readLogLevel (fmap toLower -> "debug") = Just LevelDebug
readLogLevel (fmap toLower -> "info") = Just LevelInfo
readLogLevel (fmap toLower -> "warn") = Just LevelWarn
readLogLevel (fmap toLower -> "error") = Just LevelError
readLogLevel _ = Nothing

showLogLevel :: LogLevel -> String
showLogLevel LevelDebug = "Debug"
showLogLevel LevelInfo = "Info"
showLogLevel LevelWarn = "Warn"
showLogLevel LevelError = "Error"
showLogLevel LevelOther{} = "Other"

getVerbosity :: Params Maybe -> IO LogLevel
getVerbosity opts = do
    envVerbosity <- (readLogLevel =<<) <$> lookupEnv "VERBOSITY"
    pure . fromMaybe defaultVerbosity $ opts.verbosity <|> envVerbosity
  where
    defaultVerbosity = LevelWarn

parse :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (Params m)
parse f = do
    let verbosityError =
            flag' LevelError $
                short 'q' <> long "quiet" <> help "Decrease the logging verbosity level"
    let verbosityInfo =
            flag' LevelInfo $
                short 'v' <> long "verbose" <> help "Increase the logging verbosity level"
    let verbosityDebug =
            flag' LevelDebug $
                long "debug" <> help "Set the logging verbosity level to 'debug'"
    verbosity <- f $ verbosityError <|> verbosityInfo <|> verbosityDebug
    sourceRemote <-
        f $
            strOption
                ( long "upstream"
                    <> metavar "UPSTREAM"
                    <> help "The name of the upstream remote; defaults to upstream"
                )
    targetRemote <-
        f $
            strOption
                ( long "origin"
                    <> metavar "ORIGIN"
                    <> help "The name of the origin remote; defaults to origin"
                )
    mainBranch <-
        f $
            strOption
                ( long "main-branch"
                    <> metavar "MAIN"
                    <> help "The name of the main branch; defaults to main"
                )
    pure Params{..}

getMainBranch :: Text -> IO Text
getMainBranch sourceRemote = do
    (status, output, _) <- run' getRef
    case status of
        ExitSuccess -> pure $ extractMainBranch output
        ExitFailure _ -> do
            run_ ["git", "fetch", sourceRemote]
            extractMainBranch <$> run getRef
  where
    getRef =
        [ "git"
        , "symbolic-ref"
        , "--short"
        , "refs/remotes/" <> sourceRemote <> "/HEAD"
        ]
    extractMainBranch :: Text -> Text
    extractMainBranch = (!! 1) . Text.split (== '/') . Text.strip

defaults :: Params Maybe -> IO (Params Identity)
defaults Params{..} = do
    verbosity' <- maybe (pure LevelWarn) pure verbosity
    sourceRemote' <- maybe (pure "upstream") pure sourceRemote
    mainBranch' <- maybe (getMainBranch sourceRemote') pure mainBranch
    targetRemote' <- maybe (pure "origin") pure targetRemote
    pure
        Params
            { verbosity = pure verbosity'
            , sourceRemote = pure sourceRemote'
            , targetRemote = pure targetRemote'
            , mainBranch = pure mainBranch'
            }
