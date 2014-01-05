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
}
