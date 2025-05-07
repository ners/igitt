module Params where

import Control.Monad.Logger (LogLevel (..))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Options.Applicative
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

defaults :: Params Maybe -> Params Identity
defaults Params{..} =
    Params
        { verbosity = maybe (pure LevelWarn) pure verbosity
        , sourceRemote = maybe (pure "upstream") pure sourceRemote
        , targetRemote = maybe (pure "origin") pure targetRemote
        , mainBranch = maybe (pure "main") pure mainBranch
        }
