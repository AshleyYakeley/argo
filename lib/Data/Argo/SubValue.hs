module Data.Argo.SubValue where
{
    import Import;
    
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
}
