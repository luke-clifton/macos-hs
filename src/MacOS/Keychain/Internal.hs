{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module MacOS.Keychain.Internal where

import qualified Language.C.Inline as C
import qualified Language.C.Types as C
import qualified Language.C.Inline.Context as C
import Foreign.C
import Foreign.Storable
import qualified Data.Map as Map

newtype OSStatus = OSStatus CInt
    deriving (Eq, Ord, Show)
    deriving newtype (Storable)

keychainCtx :: C.Context
keychainCtx = mempty
    { C.ctxTypesTable = Map.fromList
        [ (C.TypeName "OSStatus", [t|OSStatus|])
        ]
    }

