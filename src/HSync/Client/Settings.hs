module HSync.Client.Settings where

import Prelude
import System.FilePath(takeExtension)


partialFileExtension :: FilePath
partialFileExtension = "part"

isPartialFile :: FilePath -> Bool
isPartialFile = (==  partialFileExtension) . takeExtension
