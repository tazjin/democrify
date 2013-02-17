{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances #-}

--  ================================================================
--  Copyright (C) 2010 Tim Scheffler
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--  ================================================================

module HSObjC
    (
     c_NSApplicationMain,
     Id,
     StableId,
     StableValue(..),
     IOOBJC,
     OBJCError,
     OBJC(..),
     perfSel0, perfSel0',
     perfSel1, perfSel1',
     catchOBJC, runId,
     toCocoa,
     nsLog,
     freeStablePtr,
     toStableId,
     makeTarget, Action,
     setObjectValue, objectValue,
     setEnabled,
     (#),
    ) where

import Foreign hiding (newForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.StablePtr
import Control.Monad.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.Map as M
import Data.Map (Map)

data ObjcObject
type Id = Ptr ObjcObject    -- typed pointer for all NSObjects

class OBJC a where
    fromId :: Id -> IOOBJC a
    toId   :: a  -> IOOBJC Id

-- TODO: Own instance of class Error for IOOBJC
type OBJCError = String
type IOOBJC = ErrorT OBJCError IO

foreign import ccall unsafe "Foundation.h NSLog" c_NSLog :: Id -> IO ()

foreign import ccall unsafe "HSObjC_C.h retainId"         c_retainId         :: Id -> IO Id
foreign import ccall unsafe "HSObjC_C.h nsStringToUtf8"   c_nsStringToUtf8   :: Id -> IO CString
foreign import ccall unsafe "HSObjC_C.h utf8ToNSString"   c_utf8ToNSString   :: CString -> IO Id
foreign import ccall unsafe "HSObjC_C.h doubleValue"      c_doubleValue      :: Id -> IO CDouble
foreign import ccall unsafe "HSObjC_C.h numberWithDouble" c_numberWithDouble :: CDouble -> IO Id
foreign import ccall unsafe "HSObjC_C.h longValue"        c_longValue        :: Id -> IO CLong
foreign import ccall unsafe "HSObjC_C.h numberWithLong"   c_numberWithLong   :: CLong -> IO Id
foreign import ccall unsafe "HSObjC_C.h arrayWithCArray"  c_arrayWithCArray  :: (Ptr Id) -> CUInt -> IO Id
foreign import ccall unsafe "HSObjC_C.h getObjects"       c_getObjects       :: Id -> IO (Ptr Id)
foreign import ccall unsafe "HSObjC_C.h lengthOfArray"    c_len              :: Id -> IO CUInt
foreign import ccall unsafe "HSObjC_C.h getKeysAndValues" c_getKeysAndValues :: Id -> IO Id
foreign import ccall unsafe "HSObjC_C.h dictWithKeysAndValues" c_dictWithKeysAndValues :: Id -> Id -> IO Id

foreign import ccall unsafe "HSObjC_C.h newHSValue"       c_newHSValue       :: CString -> (StablePtr a) -> IO Id
foreign import ccall unsafe "HSObjC_C.h hsValue_getStablePtr" c_getStablePtr :: Id -> IO (StablePtr (StableValue a))

foreign import ccall unsafe "HSObjC_C.h connectAsTarget"  c_connectAsTarget  :: Id -> Id -> IO ()

-- key/value coding
foreign import ccall unsafe "HSObjC_C.h getValueForKey"   c_getValueForKey   :: Id -> Id -> IO Id
foreign import ccall unsafe "HSObjC_C.h setValueForKey"   c_setValueForKey   :: Id -> Id -> Id -> IO ()


{- These functions are imported "safe", because they might call back into the Haskell
   runtime during execution:
        - An autorelease might trigger a dealloc (not sure about this...), which might free some
          Haskell ForeignPtr.
        - The sending of random methods to random objects surely can trigger callbacks
          to the Haskell runtime.
-}
foreign import ccall        "Cocoa.h NSApplicationMain"   c_NSApplicationMain :: CInt -> Ptr (Ptr CChar) -> IO CInt
foreign import ccall        "HSObjC_C.h releaseId"        c_releaseId         :: Id -> IO ()
foreign import ccall        "HSObjC_C.h autoreleaseId"    c_autoreleaseId     :: Id -> IO Id
foreign import ccall        "HSObjC_C.h performMethod0"   c_performMethod0    :: CString -> Id -> IO Id
foreign import ccall        "HSObjC_C.h performMethod1"   c_performMethod1    :: CString -> Id -> Id -> IO Id
foreign import ccall        "HSObjC_C.h isKindOf"         c_isKindOf          :: Id -> CString -> IO CInt

-- exports
foreign export ccall callFunc1 :: (StablePtr (Id -> IOOBJC Id)) -> Id -> IO Id
foreign export ccall callFunc2 :: (StablePtr (Id -> Id -> IOOBJC Id)) -> Id -> Id -> IO Id




-- Some helper functions

checkNullPtr :: String -> IO Id -> IOOBJC Id
checkNullPtr msg act = do ptrId <- liftIO act
                          if ptrId == nullPtr
                             then throwError msg
                             else return ptrId

perfSel0 :: (OBJC a, OBJC b) => a -> String -> IOOBJC b
perfSel0 obj msg = do ptr <- toId obj
                      res <- liftIO $ BS.useAsCString (BS8.pack msg) $
                                \cstr -> c_performMethod0 cstr ptr
                      fromId res


perfSel1 :: (OBJC a, OBJC b, OBJC c) => a -> String -> b -> IOOBJC c
perfSel1 obj msg arg = do ptr <- toId obj
                          arg' <- toId arg
                          res <- liftIO $ BS.useAsCString (BS8.pack msg) $
                                    \cstr -> c_performMethod1 cstr ptr arg'
                          fromId res

{- Use this () versions to make the type system happy if you call methods
   with void return value.
-}
perfSel0' :: (OBJC a) => a -> String -> IOOBJC ()
perfSel0' = perfSel0

perfSel1' :: (OBJC a, OBJC b) => a -> String -> b -> IOOBJC ()
perfSel1' = perfSel1


whenKindOf :: (OBJC a) => String -> (Id -> IO a) -> Id -> IOOBJC a
whenKindOf className f ptr = do isKind <- isKindOfClass
                                if isKind
                                   then liftIO $ f ptr
                                   else throwError $ "not kind of" ++ className
    where
        isKindOfClass :: IOOBJC Bool
        isKindOfClass = liftIO $
                        BS.useAsCString (BS8.pack className) $ \cstr ->
                            do res <- c_isKindOf ptr cstr
                               case (fromIntegral res) of
                                   0         -> return False
                                   otherwise -> return True


catchOBJC :: IOOBJC Id -> IO Id
catchOBJC act = do eth <- runErrorT act
                   case eth of
                      Left err -> do nsLog' $ "(Haskell) OBJC error: " ++ err
                                     return nullPtr
                      Right  y -> return y

runId :: (OBJC a) => IOOBJC a -> IO Id
runId act = catchOBJC $ toId =<< act

toCocoa :: (OBJC a, OBJC b) => (a -> b) -> Id -> IO Id
toCocoa f anId = catchOBJC $ toId . f =<< fromId anId

nsLog' :: String -> IO ()
nsLog' x = do eth <- runErrorT $ toId (T.pack x)
              case eth of
                Left err -> return ()
                Right x  -> c_NSLog x

nsLog :: String -> IOOBJC ()
nsLog = liftIO . nsLog'

-- StableId, opaque data type for handling NSObjects
newtype StableId = StableId {
      foreignPtr :: ForeignPtr ObjcObject
    }

instance OBJC StableId where
    toId x = liftIO $
             withForeignPtr (foreignPtr x) $
                \ptr -> c_retainId ptr >>= c_autoreleaseId

    fromId ptr = liftIO $
                 do ptr' <- c_retainId ptr
                    x    <- newForeignPtr ptr' $ c_releaseId ptr'
                    return $ StableId x

-- Dummy instance for show, just to print something
instance Show StableId where
    show = const "StableId"

-- Void/Nil
instance OBJC () where
    fromId x = return ()
    toId x = return nullPtr


-- NSString handling
instance OBJC BS.ByteString where
    toId x = checkNullPtr "Could not create NSString" $
                            BS.useAsCString x c_utf8ToNSString

    fromId x = do ptr <- liftIO $ c_nsStringToUtf8 x
                  if ptr == nullPtr
                    then throwError "not a NSString value"
                    else liftIO $ BS.packCString ptr

instance OBJC T.Text where
    -- via ByteString
    toId = toId . encodeUtf8
    fromId x = return . decodeUtf8 =<< fromId x


-- NSNumber handling
instance OBJC Double where
    toId = checkNullPtr "Could not create NSNumber" .
           c_numberWithDouble . realToFrac

    fromId = whenKindOf "NSNumber" $
                liftM realToFrac . c_doubleValue

instance OBJC Int where
    toId = checkNullPtr "Could not create NSNumber" .
           c_numberWithLong . fromIntegral

    fromId = whenKindOf "NSNumber" $
                liftM fromIntegral . c_longValue

instance OBJC Bool where
    toId = toId . fromBool
        where
            fromBool :: Bool -> Int
            fromBool True  = 1
            fromBool False = 0

    fromId x = return . toBool =<< fromId x
        where
            toBool :: Int -> Bool
            toBool 0 = False
            toBool _ = True


-- NSArray handling
instance (OBJC a) => OBJC [a] where
    toId xs = mapM toId xs >>= toNSArray'
        where
          toNSArray' :: [Id] -> IOOBJC Id
          toNSArray' x = checkNullPtr "Could not create NSArray" $
                              withArrayLen x $ \len ptr ->
                                  c_arrayWithCArray ptr (fromIntegral len)

    fromId x = fromNSArray' x >>= mapM fromId
        where
          fromNSArray' :: Id -> IOOBJC [Id]
          fromNSArray' x = do ptr <- liftIO $ c_getObjects x
                              if ptr == nullPtr
                                then throwError "not a NSArray"
                                else liftIO $ do
                                  len <- c_len x
                                  res <- peekArray (fromIntegral len) ptr
                                  free ptr
                                  return res

-- example for a tuple
instance (OBJC a, OBJC b) => OBJC (a, b) where
    -- via list and StableId
    toId (a, b) = do -- wrap arguments into opaque StableId, so that we can use them in a list
                     aStId <- toStableId a
                     bStId <- toStableId b
                     toId [aStId, bStId]

    fromId x = do ys <- fromId x :: IOOBJC [StableId]
                  case ys of
                      (aStId:bStId:[]) -> do a <- toOBJC aStId
                                             b <- toOBJC bStId
                                             return (a, b)
                      otherwise        -> throwError "Wrong number of arguments for (,)"

instance (OBJC a, OBJC b, OBJC c) => OBJC (a, b, c) where
    -- via list and StableId
    toId (a, b, c) = do -- wrap arguments into opaque StableId, so that we can use them in a list
                     aStId <- toStableId a
                     bStId <- toStableId b
                     cStId <- toStableId c
                     toId [aStId, bStId, cStId]

    fromId x = do ys <- fromId x :: IOOBJC [StableId]
                  case ys of
                      (aStId:bStId:cStId:[]) -> do a <- toOBJC aStId
                                                   b <- toOBJC bStId
                                                   c <- toOBJC cStId
                                                   return (a, b, c)
                      otherwise        -> throwError "Wrong number of arguments for (,,)"


-- Dictionaries
{- We define dictionaries in Haskell as Maps, because lookup table [()] would conflict with
   the array type.
-}
instance (OBJC k, Ord k, OBJC a) => OBJC (Map k a) where
    toId x = do let y = M.toList x
                keys <- toId $ map fst y
                vals <- toId $ map snd y
                checkNullPtr "Could not create NSDictionary" $
                    c_dictWithKeysAndValues keys vals

    fromId x = do nsarray <- liftIO $ c_getKeysAndValues x
                  if nsarray == nullPtr
                      then throwError "not a NSDictionary"
                      {- That's surely not very elegant!
                      Do we have to make the way via [(StableId, StableId)] ?
                      -}
                      else do ys <- fromId nsarray :: IOOBJC [(StableId, StableId)]
                              keys <- mapM (toOBJC . fst) ys
                              vals <- mapM (toOBJC . snd) ys
                              return $ M.fromList $ zip keys vals

-- HSValues
newHSValue :: String -> a -> IOOBJC Id
newHSValue className val = checkNullPtr ("Could not create " ++ className ++ " object") $
                            BS.useAsCString (BS8.pack className) $ \cstr ->
                                c_newHSValue cstr =<< newStablePtr val

-- Arbitrary Haskell values
newtype StableValue a = StableValue {wrappedValue :: a}

instance OBJC (StableValue a) where
    toId x = newHSValue "HSValue" x

    fromId = whenKindOf "HSValue" $ \y -> deRefStablePtr =<< c_getStablePtr y


-- Functions
instance (OBJC a, OBJC b) => OBJC (a -> IOOBJC b) where
    toId f = newHSValue "HSFunc1" f'
        where
            f' :: Id -> IOOBJC Id
            f' x' = toId =<< f =<< fromId x'

    fromId = undefined

instance (OBJC a, OBJC b, OBJC c) => OBJC (a -> b -> IOOBJC c) where
    toId f = newHSValue "HSFunc2" f'
        where
            f' :: Id -> Id -> IOOBJC Id
            f' x' y' = do x <- fromId x'
                          y <- fromId y'
                          res <- f x y
                          toId res

    fromId = undefined


callFunc1 :: (StablePtr (Id -> IOOBJC Id)) -> Id -> IO Id
callFunc1 fPtr arg = catchOBJC $
        do f <- liftIO $ deRefStablePtr fPtr
           f arg

callFunc2 :: (StablePtr (Id -> Id -> IOOBJC Id)) -> Id -> Id -> IO Id
callFunc2 fPtr arg1 arg2  = catchOBJC $
        do f <- liftIO $ deRefStablePtr fPtr
           f arg1 arg2


toOBJC :: (OBJC a, OBJC b) => a -> IOOBJC b
toOBJC x = fromId =<< toId x

toStableId :: (OBJC a) => a -> IOOBJC StableId
toStableId = toOBJC



-- support for target/action

-- Cocoa actions never return a value, therefore they return type IOOBJC ().
-- StableId is the sender of the action.
type Action = StableId -> IOOBJC ()

makeTarget :: Action -> StableId -> IOOBJC ()
makeTarget f sender = do f'      <- toId f
                         sender' <- toId sender
                         liftIO $ c_connectAsTarget sender' f'

-- from HOC
x # f = f x

-- Shortcuts for often used methods

setObjectValue :: (OBJC a) =>  a -> StableId -> IOOBJC ()
setObjectValue val target = perfSel1' target "setObjectValue:" val

objectValue :: (OBJC a) => StableId -> IOOBJC a
objectValue target = perfSel0 target "objectValue"

setEnabled :: (OBJC a) => a -> StableId -> IOOBJC ()
setEnabled val target = do val' <- toId val
                           target' <- toId target
                           key' <- toId $ T.pack "enabled"
                           liftIO $ c_setValueForKey target' key' val'

{--- |Plays a track from the currently logged in Democrify session
playTrack :: T.Text -> IO ()
playTrack track = do
    err <- runErrorT $ toId track
    case err of
      Left err -> return ()
      Right trackId -> c_playTrack trackId-}