module Data.Argo.Record where
{
    import Import;
    
    newtype Record v = MkRecord [(String,v)];
    
    instance Functor Record where
    {
        fmap f (MkRecord entries) = MkRecord (fmap (\(s,v) -> (s,f v)) entries);
    };
    
    instance Foldable Record where
    {
        foldMap = foldMapDefault;
    };
    
    instance Traversable Record where
    {
        traverse afb (MkRecord entries) = fmap MkRecord (traverse (\(s,a) -> fmap (\b -> (s,b)) (afb a)) entries);
    };
    
    matchRecord :: Record v -> String -> [v];
    matchRecord (MkRecord entries) key = mapMaybe (\(s,v) -> if s == key then Just v else Nothing) entries;
    
    lookupRecord :: Record v -> String -> Maybe v;
    lookupRecord record key = case matchRecord record key of
    {
        (v:_) -> Just v;
        _ -> Nothing;
    };
    
    lookupRecordUnique :: Record v -> String -> Maybe v;
    lookupRecordUnique record key = case matchRecord record key of
    {
        [v] -> Just v;
        _ -> Nothing;
    };
}
