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
    
    class FromValue value t where
    {
        fromValueMaybe :: (?context :: String) => value -> Maybe t;
    };
    
    type SubValue value t = (FromValue value t,ToValue value t);

    isValue :: (Eq t,FromValue value t,?context :: String) => t -> value -> Bool;
    isValue t v = fromValueMaybe v == Just t;

    fromValue :: (FromValue value t,?context :: String) => value -> t;
    fromValue v = case fromValueMaybe v of
    {
        Just t -> t;
        Nothing -> errorC "wrong type";
    };

    fromValueM :: (Monad m,FromValue value t,?context :: String) => value -> m t;
    fromValueM v = case fromValueMaybe v of
    {
        Just t -> return t;
        Nothing -> failC "wrong type";
    };
    
    applyValue :: (FromValue v (v -> v),?context :: String) => v -> v -> v;
    applyValue f a = case fromValueMaybe f of
    {
        Just ff -> ff a;
        Nothing -> errorC "non-function application";
    };
}
