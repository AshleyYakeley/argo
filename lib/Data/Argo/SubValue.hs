module Data.Argo.SubValue where
{
    import Import;
    import Data.Argo.Number;
    
    failC :: (Monad m,?context :: String) => String -> m a;
    failC s = fail (?context ++ ": " ++ s);
    
    errorC :: (?context :: String) => String -> a;
    errorC s = error (?context ++ ": " ++ s);
    
    class ToValue value t where
    {
        toValue :: (?context :: String) => t -> value;
    };
    
    typeFail :: forall m value a. (Monad m,Show value,?context :: String) => String -> value -> m a;
    typeFail typeName v = failC ((show v) ++ " is not of type " ++ typeName);
    
    class FromValue value t where
    {
        fromValueMaybe :: forall m. (Applicative m,Monad m,?context :: String) => value -> m t;
    };
    
    type SubValue value t = (FromValue value t,ToValue value t);

    isValue :: (Eq t,FromValue value t,?context :: String) => t -> value -> Bool;
    isValue t v = fromValueMaybe v == Just t;

    fromValue :: (FromValue value t,Show value,?context :: String) => value -> t;
    fromValue v = case fromValueMaybe v of
    {
        Right t -> t;
        Left err -> error err;
    };

    applyValue :: (FromValue value (value -> value),Show value,?context :: String) => value -> value -> value;
    applyValue = fromValue;
    
    instance (ToValue v a,ToValue v ()) => ToValue v (Maybe a) where
    {
        toValue Nothing = toValue ();
        toValue (Just a) = toValue a;
    };
    
    instance (Show v,FromValue v a,FromValue v ()) => FromValue v (Maybe a) where
    {
        fromValueMaybe v = case fromValueMaybe v of
        {
            Just a -> return (Just a);
            Nothing -> case fromValueMaybe v of
            {
                Just () -> return Nothing;
                Nothing -> typeFail "maybe" v;
            };
        };
    };
    
    instance (ToValue v a, ToValue v b) => ToValue v (Either a b) where
    {
        toValue (Left a) = toValue a;
        toValue (Right b) = toValue b;
    };
    
    instance (Show v,FromValue v a, FromValue v b) => FromValue v (Either a b) where
    {
        fromValueMaybe v = case fromValueMaybe v of
        {
            Just a -> return (Left a);
            Nothing -> case fromValueMaybe v of
            {
                Just b -> return (Right b);
                Nothing -> typeFail "either" v;
            };
        };
    };

    instance (ToValue v (a -> b), ToValue v [v]) => ToValue v [a -> b] where
    {
        toValue x = toValue (fmap toValue x :: [v]);
    };
    
    instance (Show v,FromValue v (a -> b), FromValue v [v]) => FromValue v [a -> b] where
    {
        fromValueMaybe v = fmap (fmap fromValue :: [v] -> [a -> b]) (fromValueMaybe v);
    };

    instance (ToValue v ByteString, ToValue v [v]) => ToValue v [ByteString] where
    {
        toValue x = toValue (fmap toValue x :: [v]);
    };
    
    instance (Show v,FromValue v ByteString, FromValue v [v]) => FromValue v [ByteString] where
    {
        fromValueMaybe v = fmap (fmap fromValue :: [v] -> [ByteString]) (fromValueMaybe v);
    };

    instance (ToValue v String, ToValue v [v]) => ToValue v [String] where
    {
        toValue x = toValue (fmap toValue x :: [v]);
    };
    
    instance (Show v,FromValue v String, FromValue v [v]) => FromValue v [String] where
    {
        fromValueMaybe v = fmap (fmap fromValue :: [v] -> [String]) (fromValueMaybe v);
    };

    instance (ToValue v [v]) => ToValue v [[v]] where
    {
        toValue x = toValue (fmap toValue x :: [v]);
    };
    
    instance (Show v,FromValue v [v]) => FromValue v [[v]] where
    {
        fromValueMaybe v = fmap (fmap fromValue :: [v] -> [[v]]) (fromValueMaybe v);
    };

    instance (ToValue v Number) => ToValue v Integer where
    {
        toValue x = toValue (fromInteger x :: Rational);
    };
    
    instance (Show v,FromValue v Number) => FromValue v Integer where
    {
        fromValueMaybe v = do
        {
            r :: Rational <- fromValueMaybe v;
            case denominator r of
            {
                1 -> return (numerator r);
                _ -> typeFail "integer" v;
            };
        };
    };

    instance (ToValue v Number) => ToValue v Int where
    {
        toValue = toValue . toInteger;
    };
    
    instance (Show v,FromValue v Number) => FromValue v Int where
    {
        fromValueMaybe v = fmap fromInteger (fromValueMaybe v);
    };

    instance (ToValue v Number) => ToValue v Word32 where
    {
        toValue = toValue . toInteger;
    };
    
    instance (Show v,FromValue v Number) => FromValue v Word32 where
    {
        fromValueMaybe v = fmap fromInteger (fromValueMaybe v);
    };

    instance (ToValue v Number) => ToValue v Int32 where
    {
        toValue = toValue . toInteger;
    };
    
    instance (Show v,FromValue v Number) => FromValue v Int32 where
    {
        fromValueMaybe v = fmap fromInteger (fromValueMaybe v);
    };

    instance (ToValue v Number) => ToValue v Int64 where
    {
        toValue = toValue . toInteger;
    };
    
    instance (Show v,FromValue v Number) => FromValue v Int64 where
    {
        fromValueMaybe v = fmap fromInteger (fromValueMaybe v);
    };

    instance (ToValue v Number) => ToValue v CInt where
    {
        toValue (CInt x) = toValue x;
    };

    instance (Show v,FromValue v Number) => FromValue v CInt where
    {
        fromValueMaybe v = fmap CInt (fromValueMaybe v);
    };
}
