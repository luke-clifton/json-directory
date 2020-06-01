{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
module Data.JSON.Directory
    ( decodeDirectory
    , ModifiedWhileReading
    ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Internal (IResult(..), formatError, ifromJSON)
import Data.Aeson.Parser.Internal (eitherDecodeStrictWith, jsonEOF)
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.HashMap.Strict
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
import System.FilePath

data ModifiedWhileReading = ModifiedWhileReading FilePath
    deriving (Show)

instance Exception ModifiedWhileReading

data NoRuleFor = NoRuleFor FilePath
    deriving Show

instance Exception NoRuleFor

data Rule = Rule
    { predicate :: FilePath -> Bool
    , jsonKey   :: FilePath -> Text
    , parser    :: FilePath -> IO (IResult Value)
    }

jsonRule :: Rule
jsonRule = Rule
    { predicate = isSuffixOf ".json"
    , jsonKey   = Text.pack . takeBaseName
    , parser    = idecodeFileStrict
    }

textRule :: Rule
textRule = Rule
    { predicate = const True
    , jsonKey   = Text.pack . takeFileName
    , parser    = fmap (ISuccess . String) . Text.readFile
    }

defaultRules :: [Rule]
defaultRules = [jsonRule, textRule]

data EntryType
    = Directory
    | File (FilePath -> IO (IResult Value))

pathType :: [Rule] -> FilePath -> IO (Text, EntryType)
pathType rules p = do
    doesDirectoryExist p >>= \case
        True -> pure (Text.pack $ takeFileName p, Directory)
        False -> case find (\r -> predicate r p) rules of
            Nothing   -> throwIO $ NoRuleFor p
            Just rule -> pure (jsonKey rule p, File (parser rule))

decodeDirectoryValue :: MonadIO io => [Rule] -> FilePath -> io (IResult Value)
decodeDirectoryValue rules path = liftIO $ do
    time <- getModificationTime path
    ents <- listDirectory path
    kvs <- catMaybes <$> forM ents \ent -> do
        if "." `isPrefixOf` ent
        then pure Nothing
        else Just <$> do
            let path' = path </> ent
            pathType rules path' >>= \case
                (n, Directory) -> (n,) . addContext n <$> decodeDirectoryValue rules path'
                (n, File parser) -> (n,) . addContext n <$> parser path'
    time2 <- getModificationTime path
    unless (time == time2) $ throwIO (ModifiedWhileReading path)
    pure $ Object <$> sequence (Data.HashMap.Strict.fromList kvs)

addContext :: Text -> IResult a -> IResult a
addContext c (IError p s) = IError (Key c : p) s
addContext _ x = x

idecodeFileStrict :: (FromJSON a) => FilePath -> IO (IResult a)
idecodeFileStrict =
    fmap (toIResult . eitherDecodeStrictWith jsonEOF ifromJSON) . BS.readFile
  where
    toIResult (Left (p, s)) = IError p s
    toIResult (Right a) = ISuccess a

resultToEither :: IResult a -> Either String a
resultToEither (ISuccess a) = Right a
resultToEither (IError p s) = Left $ formatError p s

-- | Takes a directory and decodes it using a @`FromJSON`@ instance.
-- Each entry in the directory becomes a key, and the contents become
-- the corresponding value.
--
-- * Directories are recursed into.
-- * Files ending in @.json@ are decoded as JSON values.
-- * Everything else is assumed to be a valid unicode string.
--
-- This function can throw IO exceptions as well as a @`ModifiedWhileReading`@
-- exception if the modification time changes during processing.
decodeDirectory :: (FromJSON a, MonadIO io) => FilePath -> io (Either String a)
decodeDirectory = decodeDirectory' defaultRules

decodeDirectory' :: (FromJSON a, MonadIO io) => [Rule] -> FilePath -> io (Either String a)
decodeDirectory' rules p = do
    ev <- decodeDirectoryValue rules p
    pure . resultToEither $ do
        v <- ev
        ifromJSON v
