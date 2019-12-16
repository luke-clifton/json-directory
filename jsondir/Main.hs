{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Data.Aeson
import Control.Monad
import System.Environment
import Data.JSON.Directory
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    as <- getArgs
    forM_ as \a -> do
        decodeDirectory @Value a >>= \case
            Left e -> error e
            Right v -> B.putStrLn $ encode v

