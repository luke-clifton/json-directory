{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Key
import Data.Foldable
import Data.JSON.Directory
import Data.List (isSuffixOf)
import Data.Maybe (isNothing)
import System.Environment
import System.Exit
import System.IO
import System.FilePath
import System.Process
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as Text

parseArgs :: Bool -> Bool -> [String] -> ExceptT (Maybe String) (Writer [Rule]) [FilePath]
parseArgs b c ("--rule":rest) = case rest of
    pat:filter:rest -> do
        let
            rule = Rule
                { predicate = isSuffixOf pat
                , jsonKey   = Data.Aeson.Key.fromString . takeBaseName
                , parser    =
                    let
                        parse fp = do
                            s <- readFile fp
                            r <- readCreateProcess (shell filter) s
                            pure $ idecodeStrict (BS.pack r)
                        handleFailure :: IOError -> IO (IResult Value)
                        handleFailure = pure . IError [] . displayException
                    in
                        \fp -> catch (parse fp) handleFailure
                }
        tell [rule]
        parseArgs b c rest
    _ -> throwError $ Just "Invalid argument. Expected `--rule <SUFFIX> <FILTER>`"
parseArgs b c ("--no-default-json":rest) = parseArgs False c rest
parseArgs b c ("--default-json":rest) = parseArgs True c rest
parseArgs b c ("--no-default-text":rest) = parseArgs b False rest
parseArgs b c ("--default-text":rest) = parseArgs b True rest
parseArgs b c ("--no-defaults":rest) = parseArgs False False rest
parseArgs _ _ ("--help":_) = throwError Nothing
parseArgs b c ("--":rest) = do
    when b (tell [jsonRule])
    when c (tell [textRule])
    pure rest
parseArgs b c (a:as) = (a:) <$> parseArgs b c as
parseArgs b c [] = do
    when b (tell [jsonRule])
    when c (tell [textRule])
    pure []

usage :: String
usage = "\
\jsondir [--help] [--rule <SUFFIX> <FILTER> ...]\n\
\        [--[no-]default{s,-json,-text}] <ROOT> ...\n\
\\n\
\  Turn a directory structure into a JSON value\n\
\\n\
\ --rule <SUFFIX> <FILTER>     Filter the contents of files with the given\n\
\                              SUFFIX with FILTER. The default rules use .json\n\
\                              files as is, and treat everything else as strings.\n\
\                              Can be specified multiple times. Rule are tried in\n\
\                              the order specified.\n\
\ --[no-]defaults              Enable or disable the default rules. Default on.\n\
\ --[no-]default-json          Enable or disable the .json file rule\n\
\ --[no-]default-text          Enable or disable the raw file to JSON string rule.\n\
\ <ROOT>                       Directory root to turn into a JSON value\n\
\\n\
\ EXAMPLE\n\
\  jsondir --rule '.yml' yaml2json ./my-dir\n\
\"

main :: IO ()
main = do
    (eRoots, rules) <- runWriter . runExceptT . parseArgs True True <$> getArgs

    case eRoots of
        Left err -> do
            traverse_ (hPutStrLn stderr) err
            if isNothing err
            then putStrLn usage
            else do
                hPutStrLn stderr "Usage:"
                hPutStrLn stderr usage
            exitFailure
        Right roots -> do
            forM_ roots \a -> do
                decodeDirectory' @Value rules a >>= \case
                    Left e -> do
                        hPutStrLn stderr $ "Error: " ++ e
                        exitFailure
                    Right v -> B.putStrLn $ encode v

