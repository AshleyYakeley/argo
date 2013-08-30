module Data.Argo.SubValue where
{
    import Import;
    
    class SubValue value t where
    {
        toValue :: t -> value;
        fromValueMaybe :: value -> Maybe t;
    };

    isValue :: (Eq t,SubValue value t) => t -> value -> Bool;
    isValue t v = fromValueMaybe v == Just t;

    fromValue :: (SubValue value t) => value -> t;
    fromValue v = case fromValueMaybe v of
    {
        Just t -> t;
        Nothing -> error "wrong type";
    };
    
    applyValue :: (SubValue v (v -> v)) => v -> v -> v;
    applyValue f a = case fromValueMaybe f of
    {
        Just ff -> ff a;
        Nothing -> error "non-function application";
    };
}
