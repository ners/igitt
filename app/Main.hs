module Main where

import Bump qualified
import Command (Command (..))
import Command qualified
import Data.Tuple.Extra (firstM)
import New qualified
import Options.Applicative
    ( ParserInfo
    , execParser
    , fullDesc
    , helper
    , info
    , optional
    , progDesc
    )
import Params (Params, defaults)
import Params qualified
import Push qualified
import Prelude

parserInfo :: ParserInfo (Params Maybe, Command Maybe)
parserInfo =
    info
        ( helper <*> do
            params <- Params.parse optional
            command <- Command.parse optional
            pure (params, command)
        )
        (fullDesc <> progDesc "igitt: an icky git tool")

main :: IO ()
main = do
    (params, command) <- firstM defaults =<< execParser parserInfo
    case command of
        New newParams -> New.new params newParams
        Push pushParams -> void $ Push.push params pushParams
        Bump bumpParams -> void $ Bump.bump params bumpParams
