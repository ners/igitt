module WorkingBranch where

import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty.Extra (maximumOn1)
import Data.Text qualified as Text
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Prelude

data WorkingBranch = WorkingBranch
    { base :: Text
    , n :: Natural
    }

succBranch :: WorkingBranch -> WorkingBranch
succBranch b@WorkingBranch{..} = b{n = succ n}

parseBranch :: Text -> Either Text WorkingBranch
parseBranch branchName =
    case groups of
        [base, Text.unpack -> readMaybe -> Just n] -> Right WorkingBranch{..}
        _ -> Left branchName
  where
    re = "^([A-Za-z0-9_-]+)-([0-9]+)$" :: Text
    (_, _, _, groups) = branchName =~ re :: (Text, Text, Text, [Text])

showBranch :: WorkingBranch -> Text
showBranch WorkingBranch{..} = Text.intercalate "-" [base, ishow n]

showPrBranch :: WorkingBranch -> Text
showPrBranch WorkingBranch{..} = Text.intercalate "-" [base, "pr"]

getCurrBranch :: IO Text
getCurrBranch = run ["git", "branch", "--show-current"]

getCurrWorkingBranch :: IO WorkingBranch
getCurrWorkingBranch =
    either (\t -> fatalError $ "Expected working branch, got: " <> t) pure
        . parseBranch
        =<< getCurrBranch

switchToNextWorkingBranch :: WorkingBranch -> IO ()
switchToNextWorkingBranch currBranch =
    run_ ["git", "checkout", "-b", showBranch nextBranch, showBranch currBranch]
  where
    nextBranch = succBranch currBranch

findLatest :: Text -> IO (Maybe WorkingBranch)
findLatest base = do
    allBranches <-
        stripPrefix "refs/heads/"
            . (!! 1)
            . Text.split (== ' ')
            . Text.strip
            <$$> Text.lines
            <$> run ["git", "show-ref", "--branches"]
    let parsedBranches = mapMaybe (eitherToMaybe . parseBranch) allBranches
    let matchingBranches = filter ((== base) . (.base)) parsedBranches
    pure $ maximumOn1 (.n) <$> nonEmpty matchingBranches
