module Command where

import New
import Options.Applicative
    ( CommandFields
    , Mod
    , command
    , hsubparser
    , info
    , progDesc
    )
import Push
import Prelude

data Command m
    = New (NewParams m)
    | Push (PushParams m)
    | Bump (PushParams m)
    deriving stock (Generic)

parse
    :: forall m
     . (forall a. Parser a -> Parser (m a))
    -> Parser (Command m)
parse f = hsubparser $ mconcat [parseNew f, parsePush f, parseBump f]

parseNew
    :: forall m
     . (forall a. Parser a -> Parser (m a))
    -> Mod CommandFields (Command m)
parseNew f =
    command "new" $
        info
            (New <$> New.parseParams f)
            (progDesc "Create a new numbered branch")

parsePush
    :: forall m
     . (forall a. Parser a -> Parser (m a))
    -> Mod CommandFields (Command m)
parsePush f =
    command "push" $
        info
            (Push <$> Push.parseParams f)
            (progDesc "Push the current numbered branch")

parseBump
    :: forall m
     . (forall a. Parser a -> Parser (m a))
    -> Mod CommandFields (Command m)
parseBump f =
    command "bump" $
        info
            (Bump <$> Push.parseParams f)
            (progDesc "Push the current numbered branch and bump its number")
