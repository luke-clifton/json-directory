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

data EntryType
    = Directory
    | JSON
    | TextFile

pathType :: FilePath -> IO (Text, EntryType)
pathType p = do
    doesDirectoryExist p >>= \case
        True -> pure (Text.pack $ takeFileName p, Directory)
        False -> pure case splitExtension (takeFileName p) of
            (name, ".json") -> (Text.pack $ name, JSON)
            _               -> (Text.pack $ takeFileName p, TextFile)

decodeDirectoryValue :: MonadIO io => FilePath -> io (IResult Value)
decodeDirectoryValue path = liftIO $ do
    time <- getModificationTime path
    ents <- listDirectory path
    kvs <- catMaybes <$> forM ents \ent -> do
        if "." `isPrefixOf` ent
        then pure Nothing
        else Just <$> do
            let path' = path </> ent
            pathType path' >>= \case
                (n, Directory) -> (n,) . addContext n <$> decodeDirectoryValue path'
                (n, JSON     ) -> (n,) . addContext n <$> idecodeFileStrict path'
                (n, TextFile ) -> (n,) . ISuccess . String <$> Text.readFile path'
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
decodeDirectory p = do
    ev <- decodeDirectoryValue p
    pure . resultToEither $ do
        v <- ev
        ifromJSON v
