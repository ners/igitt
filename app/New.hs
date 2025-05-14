module New where

import Control.Monad (unless)
import Data.Maybe (isNothing)
import Params (Params (..))
import WorkingBranch
import Prelude

{-
 - igitt new <name>
 -     if any <name>-<N> already exists, it FAILS
 -     otherwise,
 -         it creates <name>-1 from upstream/master
 -         creates an empty commit (asks the user for a message)
 -}

data ExistingBranchAction
    = IncrementAndContinue
    | Continue
    | IncrementAndNew
    | ChooseDifferentName
    deriving stock (Eq)

instance Show ExistingBranchAction where
    show IncrementAndContinue = "Increment and continue"
    show Continue = "Continue"
    show IncrementAndNew = "Increment and new"
    show ChooseDifferentName = "Choose different name"

parseExistingBranchAction :: Parser ExistingBranchAction
parseExistingBranchAction =
    incrementAndContinue
        <|> continue Continue
        <|> increment IncrementAndNew
        <|> noExists
  where
    increment, continue :: a -> Parser a
    increment a = flag' a (long "increment")
    continue a = flag' a (long "continue")
    noExists = flag' ChooseDifferentName (long "no-exists")
    incrementAndContinue = continue () *> increment IncrementAndContinue

askExistingBranchAction :: IO ExistingBranchAction
askExistingBranchAction = buttons "Existing branch with this name found, wat do?" options 0 ishow
  where
    options :: [(ExistingBranchAction, Char)]
    options =
        [ (IncrementAndContinue, 'I')
        , (Continue, 'C')
        , (IncrementAndNew, 'N')
        , (ChooseDifferentName, 'D')
        ]

data NewParams m = NewParams
    { branchName :: m Text
    , commitMessage :: m Text
    , existingAction :: m ExistingBranchAction
    }

parseParams
    :: forall m
     . (forall a. Parser a -> Parser (m a))
    -> Parser (NewParams m)
parseParams f = do
    branchName <-
        f . strArgument $
            metavar "BRANCH" <> help "The base name of the branch to create"
    commitMessage <-
        f . strOption $
            long "message"
                <> short 'm'
                <> metavar "MESSAGE"
                <> help "The commit message for the initial commit"
    existingAction <- f parseExistingBranchAction
    pure NewParams{..}

askParams :: NewParams Maybe -> NewParams IO
askParams NewParams{..} =
    NewParams
        { branchName = maybe (textInput "Branch name:" "") pure branchName
        , commitMessage =
            maybe (commitMessageInput "") pure commitMessage
        , existingAction = maybe askExistingBranchAction pure existingAction
        }

new :: Params Identity -> NewParams Maybe -> IO ()
new params newParams@(askParams -> newParamsIO@NewParams{..}) = do
    base <- branchName
    findLatest base >>= \case
        Nothing -> createWorkingBranch params newParamsIO WorkingBranch{base, n = 1}
        Just wb -> do
            let errorMsg = "Branch already exists: " <> showBranch wb
            existingAction >>= \case
                IncrementAndContinue -> run_ ["git", "checkout", "-b", showBranch $ succBranch wb, showBranch wb]
                Continue -> run_ ["git", "checkout", showBranch wb]
                IncrementAndNew -> createWorkingBranch params newParamsIO $ succBranch wb
                ChooseDifferentName | isNothing newParams.branchName -> do
                    unless (isNothing newParams.existingAction) $ printWarn errorMsg
                    new params newParams
                ChooseDifferentName -> fatalError errorMsg

createWorkingBranch :: Params Identity -> NewParams IO -> WorkingBranch -> IO ()
createWorkingBranch Params{..} NewParams{commitMessage} wb = do
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
        [ "git"
        , "commit"
        , "--allow-empty"
        , "--allow-empty-message"
        , "--message"
        , msg
        ]
