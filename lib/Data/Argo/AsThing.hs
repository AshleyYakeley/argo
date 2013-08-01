module Data.Thing.AsThing where
{
    import Import;
    import Data.Thing.Thing;
    import qualified Data.Map;
    
    class AsThing a where
    {
        toThing :: a -> Thing;
        fromThing :: Thing -> Maybe a;

        toThingList :: [a] -> Thing;
        toThingList aa = MkThing "" (fmap (\a -> ("",toThing a)) aa);
   
        fromThingList :: Thing -> Maybe [a];
        fromThingList (MkThing "" ff) = for ff (\(k,t) -> case k of
            {
                "" -> fromThing t;
                _ -> Nothing;
            });
        fromThingList _ = Nothing;
    };
    
    instance AsThing Thing where
    {
        toThing = id;
        fromThing = Just;
    };
    
    instance AsThing Char where
    {
        toThing c = MkThing [c] [];
        fromThing (MkThing [c] []) = Just c;
        fromThing _ = Nothing;

        -- special handling of String
        toThingList s = MkThing s [];        
        fromThingList (MkThing s []) = Just s;
        fromThingList _ = Nothing;
    };
    
    instance AsThing a => AsThing [a] where
    {
        toThing aa = toThingList aa;
        fromThing t = fromThingList t;
    };
    
    instance (AsThing a,AsThing b) => AsThing (a,b) where
    {
        toThing (a,b) = MkThing "" [("",toThing a),("",toThing b)];
        fromThing (MkThing "" [("",ta),("",tb)]) = do
        {
            a <- fromThing ta;
            b <- fromThing tb;
            return (a,b);
        };
        fromThing _ = Nothing;
    };
    
    instance AsThing a => AsThing (Maybe a) where
    {
        toThing Nothing = MkThing "" [];
        toThing (Just a) = MkThing "" [("",toThing a)];
        
        fromThing (MkThing "" []) = Just Nothing;
        fromThing (MkThing "" [("",t)]) = fromThing t;
        fromThing _ = Nothing;
    };
    
    instance AsThing () where
    {
        toThing () = MkThing "" [];
        fromThing (MkThing "" []) = Just ();
        fromThing _ = Nothing;
    };
    
    instance AsThing Integer where
    {
        toThing x = MkThing (show x) [];
        fromThing (MkThing s []) = readMaybe s;
        fromThing _ = Nothing;
    };
    
    instance (AsThing a) => AsThing (Data.Map.Map String a) where
    {
        toThing map = MkThing "" (fmap (\(k,a) -> (k,toThing a)) (Data.Map.toList map));
        fromThing (MkThing "" ff) = fmap Data.Map.fromList (for ff (\(k,t) -> fmap (\a -> (k,a)) (fromThing t)));
    };
}
