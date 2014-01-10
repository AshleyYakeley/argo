module Data.Argo.Object where
{
    import Import;
    
    newtype Object v = MkObject {unObject :: [(String,v)]};
    
    instance Functor Object where
    {
        fmap f (MkObject entries) = MkObject (fmap (\(s,v) -> (s,f v)) entries);
    };
    
    instance Foldable Object where
    {
        foldMap = foldMapDefault;
    };
    
    instance Traversable Object where
    {
        traverse afb (MkObject entries) = fmap MkObject (traverse (\(s,a) -> fmap (\b -> (s,b)) (afb a)) entries);
    };
    
    objectMatch :: Object v -> String -> [v];
    objectMatch (MkObject entries) key = mapMaybe (\(s,v) -> if s == key then Just v else Nothing) entries;
    
    objectLookup :: Object v -> String -> Maybe v;
    objectLookup object key = case objectMatch object key of
    {
        (v:_) -> Just v;
        _ -> Nothing;
    };
    
    objectLookupUnique :: Object v -> String -> Maybe v;
    objectLookupUnique object key = case objectMatch object key of
    {
        [v] -> Just v;
        _ -> Nothing;
    };
}
