module Prelude
    ( module Prelude
    , module Control.Applicative
    , module Control.Monad.Extra
    , module Control.Monad.Logger
    , module Data.Either.Extra
    , module Data.Functor
    , module Data.Functor.Identity
    , module Data.List.NonEmpty
    , module Data.Maybe
    , module Data.String
    , module Data.Text
    , module GHC.Generics
    , module Numeric.Natural
    , module Options.Applicative
    , module System.Exit
    )
where

import Control.Applicative ((<|>))
import Control.Monad.Extra (whenM)
import Control.Monad.Logger (LogLevel)
import Data.Either.Extra (eitherToMaybe)
import Data.Functor
import Data.Functor.Identity
import Data.List.Extra ((!?))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Rope.Zipper qualified as RopeZipper
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Options.Applicative
    ( Mod
    , OptionFields
    , Parser
    , completeWith
    , flag'
    , help
    , long
    , maybeReader
    , metavar
    , option
    , short
    , showDefault
    , strArgument
    , strOption
    )
import System.Console.ANSI
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (stderr)
import System.Process qualified as Process
import System.Terminal.Widgets.Buttons
import System.Terminal.Widgets.Common (runWidgetIO)
import System.Terminal.Widgets.TextInput
import Text.Read (readMaybe)
import "base" Prelude hiding (unzip)

infixl 4 <$$>

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

catEithers :: [Either e a] -> [a]
catEithers = mapMaybe eitherToMaybe

fatalError :: Text -> IO a
fatalError t = printError t >> exitFailure

printError :: Text -> IO ()
printError t = do
    hSetSGR stderr [SetColor Foreground Vivid Red]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printWarn :: Text -> IO ()
printWarn t = do
    hSetSGR stderr [SetColor Foreground Vivid Yellow]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printInfo :: Text -> IO ()
printInfo t = do
    hSetSGR stderr [SetColor Foreground Dull Cyan]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printDebug :: Text -> IO ()
printDebug t = do
    hSetSGR stderr [SetColor Foreground Dull Magenta]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

run' :: NonEmpty Text -> IO (ExitCode, Text, Text)
run' (x :| xs) = do
    printInfo $ Text.unwords (x : xs)
    (exitCode, out, err) <-
        Process.readProcessWithExitCode (Text.unpack x) (Text.unpack <$> xs) ""
    pure (exitCode, fromString out, fromString err)

run :: NonEmpty Text -> IO Text
run xs =
    run' xs >>= \case
        (ExitSuccess, out, _) -> pure out
        (code, _, _) -> exitWith code

run_ :: NonEmpty Text -> IO ()
run_ = void . run

textInputOpts :: Bool -> Bool -> Text -> Text -> IO Text
textInputOpts multiline required ((<> " ") -> prompt) (RopeZipper.fromText -> value) = do
    text <- runWidgetIO TextInput{valueTransform = id, ..}
    pure $ RopeZipper.toText text.value

textInput :: Text -> Text -> IO Text
textInput = textInputOpts False True

multilineTextInput :: Text -> Text -> IO Text
multilineTextInput = textInputOpts True False

commitMessageInput :: Text -> IO Text
commitMessageInput = textInput "Commit message: "

buttons :: (Eq a, Show a) => Text -> [(a, Char)] -> Int -> (a -> Text) -> IO a
buttons prompt values selected buttonText = do
    b <-
        runWidgetIO
            Buttons
                { prompt
                , buttons = [(buttonText s, Just c) | (s, c) <- values]
                , selected
                }
    case values !? b.selected of
        Just (a, _) -> pure a
        Nothing -> do
            printError "Invalid selection"
            buttons prompt values selected buttonText

parseYesNo :: String -> String -> Parser Bool
parseYesNo yesLong yesHelp = flag' True (long yesLong <> help yesHelp) <|> flag' False (long noLong)
  where
    noLong = "no-" <> yesLong

yesNoButtons :: Text -> Bool -> IO Bool
yesNoButtons prompt defaultValue = do
    let values = [("Yes", 'Y'), ("No", 'N')] :: [(Text, Char)]
    let selected = if defaultValue then 0 else 1
    ("Yes" ==) <$> buttons prompt values selected id

parseChoice
    :: forall a
     . (Show a, Read a)
    => Mod OptionFields a
    -> [a]
    -> Parser a
parseChoice m xs = option (maybeReader readMaybe) $ m <> showDefault <> completeWith options
  where
    options = show @a <$> xs

parseEnum
    :: (Bounded a, Enum a, Show a, Read a)
    => Mod OptionFields a
    -> Parser a
parseEnum = flip parseChoice [minBound .. maxBound]

stripPrefix :: Text -> Text -> Text
stripPrefix prefix t = fromMaybe t $ Text.stripPrefix prefix t
