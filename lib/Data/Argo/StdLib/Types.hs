{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Argo.StdLib.Types where
{
    import Import;
    import System.Posix.Types;
    import Data.Argo.Value;
    
    instance FromValue CUid where
    {
        fromValueMaybe v = fmap CUid (fromValueMaybe v);
    };
    
    instance ToValue CUid where
    {
        toValue (CUid uid) = toValue uid;
    };
    
    instance FromValue CGid where
    {
        fromValueMaybe v = fmap CGid (fromValueMaybe v);
    };
    
    instance ToValue CGid where
    {
        toValue (CGid uid) = toValue uid;
    };
    
    instance ToValue COff where
    {
        toValue (COff uid) = toValue uid;
    };
}
