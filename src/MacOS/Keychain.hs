{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module MacOS.Keychain
    ( getPassword
    , setPassword
    , User(..)
    , Service(..)
    , Password(..)
    ) where

import qualified Language.C.Inline as C
import Control.Exception
import Control.Monad
import Data.ByteString
import Data.String
import Data.Monoid
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import MacOS.Keychain.Internal

C.context (C.baseCtx <> C.bsCtx <> keychainCtx)

C.include "<Security/Security.h>"
C.include "<CoreFoundation/CoreFoundation.h>"

newtype User = User ByteString
    deriving (Eq, Ord, Show)
    deriving newtype (IsString)

newtype Service = Service ByteString
    deriving (Eq, Ord, Show)
    deriving newtype (IsString)

newtype Password = Password ByteString
    deriving newtype (IsString)

instance Show Password where
    show _ = "Password <hidden>"

data OSError = OSError OSStatus ByteString

instance Show OSError where
    show = displayException

instance Exception OSError where
    displayException (OSError s m) = show s ++ ": " ++ show m

osStatusMessage :: OSStatus -> IO ByteString
osStatusMessage osstatus = do
    let maxSize = 1000
    allocaBytes (fromIntegral maxSize) $ \msg -> do
        suc <- [C.block|
            int {
                CFStringRef sref = SecCopyErrorMessageString($(OSStatus osstatus), NULL);
                return CFStringGetCString(sref, $(char * msg), $(int maxSize), kCFStringEncodingUTF8);
            }
        |]
        when (suc == 0) $ error "Keychain.hs: failed to allocate enough room for OSStatus message"
        packCString msg;

throwOSStatus :: OSStatus -> IO ()
throwOSStatus (OSStatus 0) = pure ()
throwOSStatus s = do
    msg <- osStatusMessage s
    throwIO (OSError s msg)

setPassword :: Service -> User -> Password -> IO ()
setPassword (Service service) (User user) (Password password) = do
    res <- [C.block|
        OSStatus {
            SecKeychainItemRef ref;
            OSStatus res = SecKeychainFindGenericPassword
                ( NULL
                , $bs-len:service
                , $bs-ptr:service
                , $bs-len:user
                , $bs-ptr:user
                , NULL
                , NULL
                , &ref
                );
            if (res != errSecSuccess)
            {
                return res;
            }
            return SecKeychainItemModifyContent(ref, NULL, $bs-len:password, $bs-ptr:password);
        }
    |]
    throwOSStatus res


-- | Get a generic password from the Keychain. Pass the empty string for
-- the username to match any username.
getPassword :: Service -> User -> IO Password
getPassword (Service service) (User user) = alloca (alloca . go)
  where
    go :: Ptr CUInt -> Ptr CString -> IO Password
    go size pw = do
        res <- [C.block|
            OSStatus {
                return SecKeychainFindGenericPassword
                    ( NULL
                    , $bs-len:service
                    , $bs-ptr:service
                    , $bs-len:user
                    , $bs-ptr:user
                    , $(unsigned int *size)
                    , (void **)$(char ** pw)
                    , NULL
                    );
            }
        |]
        throwOSStatus res
        s <- peek size
        p <- peek pw
        p' <- packCStringLen (p, fromIntegral s)
        [C.exp| void {SecKeychainItemFreeContent(NULL, *$(char ** pw));} |]
        pure $ Password p'
