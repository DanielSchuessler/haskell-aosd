{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module Graphics.Aosd.CallbackUtil(UserData,Callback,UniversalCallback,mkUniversalCallback,tunnelCallback) where

import Foreign
import Graphics.Aosd.Util


type UserData = Ptr ()


type Callback a = a -> UserData -> IO ()

-- | A callback which assumes that its 'UserData' argument is a @'StablePtr' (a -> IO ())@ and
-- delegates the @a@ to that @a -> IO ()@.
newtype UniversalCallback a = UniversalCallback (FunPtr (Callback a))

mkUniversalCallback :: forall a. 
       (Callback a -> IO (FunPtr (Callback a))) 
        -- ^ Should be something obtained from a @foreign import ccall \"wrapper\" ...@ declaration 
    -> IO (UniversalCallback a)
mkUniversalCallback foreignImportWrapper = 
        UniversalCallback `fmap` foreignImportWrapper universalCallback

universalCallback :: a -> Ptr () -> IO ()
universalCallback a userData = do
            handler <- deRefStablePtr (castPtrToStablePtr userData) :: IO (a -> IO ()) 
            handler a

tunnelCallback :: 
        UniversalCallback a 
     -> (FunPtr (Callback a) -> UserData -> IO ()) -- ^ The C-imported callback setter function 
     -> (a -> IO ()) -- ^ The haskell function you want to be called back 
     -> IO (StablePtr (a -> IO ())) -- ^ Returns a StablePtr which needs to be freed once the callback is no longer used

tunnelCallback (UniversalCallback u) setCallback haskellCallback = do
    sp <- newStablePtrDebug "tunnelCallback" "callback" haskellCallback
    setCallback u (castStablePtrToPtr sp)
    return sp

