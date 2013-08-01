module Data.Argo.Expression where
{
    import Import;
    import Data.Witness.List;
    import GHC.Exts(Constraint);

    data ConstraintWitness (constraint :: Constraint) where
    {
        MkConstraintWitness :: constraint => ConstraintWitness constraint;
    };

    class AppendList la lb where
    {
        type ListAppend la lb :: *;
        listAppendWitness :: ListType w la -> ListType w lb -> ListType w (ListAppend la lb);
        listJoin :: la -> lb -> ListAppend la lb;
        listSplit :: ListAppend la lb -> (la,lb);
    };

    instance AppendList () lb where
    {
        type ListAppend () lb = lb;
        listAppendWitness NilListType wlb = wlb;
        listJoin () lb = lb;
        listSplit lb = ((),lb);
    };

    instance (AppendList la lb) => AppendList (a,la) lb where
    {
        type ListAppend (a,la) lb = (a,ListAppend la lb);
        listAppendWitness (ConsListType wa wla) wlb = ConsListType wa (listAppendWitness wla wlb);
        listJoin (a,la) lb = (a,listJoin la lb);
        listSplit (a,lab) = case listSplit lab of
        {
            (la,lb) -> ((a,la),lb);
        };
    };

    witnessedListAppend :: ListType w la -> ListType w lb -> ConstraintWitness (AppendList la lb);
    witnessedListAppend NilListType wlb = MkConstraintWitness;
    witnessedListAppend (ConsListType wa wla) wlb = case witnessedListAppend wla wlb of
    {
        MkConstraintWitness -> MkConstraintWitness;
    };

    data RemoveFromList w a l = forall lr. MkRemoveFromList
    {
        listRemoveWitness :: ListType w lr,
        listInsert :: a -> lr -> l,
        listRemove :: l -> lr
    };

    removeAllMatching :: (SimpleWitness w) => w a -> ListType w l -> RemoveFromList w a l;
    removeAllMatching _ NilListType = MkRemoveFromList
    {
        listRemoveWitness = NilListType,
        listInsert = \_ -> id,
        listRemove = id
    };
    removeAllMatching wa (ConsListType wb rest) = case removeAllMatching wa rest of
    {
        MkRemoveFromList wit ins rem -> case matchWitness wa wb of
        {
            Just MkEqualType -> MkRemoveFromList
            {
                listRemoveWitness = wit,
                listInsert = \a l2 -> (a,ins a l2),
                listRemove = \(_,l1) -> rem l1
            };
            Nothing -> MkRemoveFromList
            {
                listRemoveWitness = ConsListType wb wit,
                listInsert = \a (b,l2) -> (b,ins a l2),
                listRemove = \(b,l1) -> (b,rem l1)
            };
        };
    };

    data RemoveManyFromList wit lx l = forall lr. MkRemoveManyFromList
    {
        listRemoveManyWitness :: ListType wit lr,
        listInsertMany :: lx -> lr -> l,
        listRemoveMany :: l -> lr
    };

    removeAllMatchingMany :: (SimpleWitness wit) => ListType wit lx -> ListType wit l -> RemoveManyFromList wit lx l;
    removeAllMatchingMany NilListType wl = MkRemoveManyFromList
    {
        listRemoveManyWitness = wl,
        listInsertMany = \_ lr -> lr,
        listRemoveMany = \l -> l
    };
    removeAllMatchingMany (ConsListType wa wlx) wl = case removeAllMatching wa wl of
    {
        MkRemoveFromList wl' ins rem -> case removeAllMatchingMany wlx wl' of
        {
            MkRemoveManyFromList wl'' insM remM -> MkRemoveManyFromList
            {
                listRemoveManyWitness = wl'',
                listInsertMany = \(a,lx) lr -> ins a (insM lx lr),
                listRemoveMany = remM . rem
            };
        };
    };



    data Expression (combine :: * -> * -> *) (wit :: * -> *) (f :: * -> *) (r :: *) where
    {
        MkExpression :: ListType wit vals -> f (combine vals r) -> Expression combine wit f r;
    };

    expressionSymbol :: f (combine (val,()) r) -> wit val -> Expression combine wit f r;
    expressionSymbol fcvr wit = MkExpression (ConsListType wit NilListType) fcvr;

    type ValueExpression = Expression (->);
    type MatchExpression = Expression (,);

    instance (Functor f) => Functor (ValueExpression wit f) where
    {
        fmap ab (MkExpression wits fcva) = MkExpression wits (fmap (fmap ab) fcva);
    };

    instance (Functor f) => Functor (MatchExpression wit f) where
    {
        fmap ab (MkExpression wits fcva) = MkExpression wits (fmap (fmap ab) fcva);
    };

    instance (Applicative f) => Applicative (ValueExpression wit f) where
    {
        pure a = MkExpression NilListType (pure (pure a));
        (MkExpression wits1 fcvab) <*> (MkExpression wits2 fcva) = case witnessedListAppend wits1 wits2 of
        {
            MkConstraintWitness -> MkExpression (listAppendWitness wits1 wits2) (liftA2 (\v1ab v2a v12 -> case listSplit v12 of
            {
                (v1,v2) -> (v1ab v1) (v2a v2);
            }) fcvab fcva);
        }
    };

    instance (Applicative f) => Applicative (MatchExpression wit f) where
    {
        pure a = MkExpression NilListType (pure (pure a));
        (MkExpression wits1 fcvab) <*> (MkExpression wits2 fcva) = case witnessedListAppend wits1 wits2 of
        {
            MkConstraintWitness -> MkExpression (listAppendWitness wits1 wits2) (liftA2 (\(v1,ab) (v2,a) -> (listJoin v1 v2,ab a)) fcvab fcva);
        }
    };

    valueSymbol :: (Applicative f) => wit val -> ValueExpression wit f val;
    valueSymbol = expressionSymbol (pure (\(val,()) -> val));

    abstract :: (SimpleWitness wit,Functor f) => wit val -> ValueExpression wit f r -> ValueExpression wit f (val -> r);
    abstract wit (MkExpression wits fvsr) = case removeAllMatching wit wits of
    {
        MkRemoveFromList newwits ins rem -> MkExpression newwits (fmap (\vsr vals a -> vsr (ins a vals)) fvsr);
    };

    letBind :: (SimpleWitness wit,Applicative f) => wit val -> ValueExpression wit f val -> ValueExpression wit f r -> ValueExpression wit f r;
    letBind wit valExp exp = (abstract wit exp) <*> valExp;


    matchBind :: (SimpleWitness wit,Functor f1,Functor f2) =>
        MatchExpression wit f1 a ->
        ValueExpression wit f2 b ->
        ValueExpression wit (Compose f1 f2) (a,b);
    matchBind (MkExpression matchWits f1vca) (MkExpression valueWits f2vtb) = case removeAllMatchingMany matchWits valueWits of
    {
        MkRemoveManyFromList newValueWits insM remM -> MkExpression newValueWits
         (Compose (fmap (\(lx,a) -> fmap (\lb lr -> (a,lb (insM lx lr))) f2vtb) f1vca));
    };


    type PatternExpression wit q = MatchExpression wit (Compose ((->) q) Maybe);

    patternSymbol :: wit val -> PatternExpression wit val ();
    patternSymbol = expressionSymbol (Compose (\val -> pure ((val,()),())));
{-
    patternCofmap :: (a -> b) -> PatternExpression wit b r -> PatternExpression wit a r;
    patternCofmap ab (ClosedExpression (Compose bmv)) = ClosedExpression (Compose (bmv . ab));
    patternCofmap ab (OpenExpression w expr) = OpenExpression w (patternCofmap ab expr);
-}
    pattern :: (q -> Maybe r) -> PatternExpression wit q r;
    pattern qmr = MkExpression NilListType (fmap (\r -> ((),r)) (Compose qmr));

    
     






    
{-
    data Expression (wit :: (* -> *) -> *) (f :: * -> *) (r :: *) where
    {
        ClosedExpression :: f r -> Expression wit f r;
        OpenExpression :: (Functor g) => wit g -> Expression wit f (g r) -> Expression wit f r;
    };

    instance (Functor f) => Functor (Expression wit f) where
    {
        fmap pq (ClosedExpression fp) = ClosedExpression (fmap pq fp);
        fmap pq (OpenExpression w egp) = OpenExpression w (fmap (fmap pq) egp);
    };

    instance (Applicative f) => Applicative (Expression wit f) where
    {
        pure t = ClosedExpression (pure t);
        (ClosedExpression fpq) <*> ep = ffmap fpq ep where
        {
            ffmap :: f (p -> q) -> Expression wit f p -> Expression wit f q;
            ffmap fpq (ClosedExpression fp) = ClosedExpression (fpq <*> fp);
            ffmap fpq (OpenExpression a egp) = OpenExpression a (ffmap (fmap fmap fpq) egp);
        };
        (OpenExpression a egpq) <*> ep = OpenExpression a (liftA2 (\p -> fmap (\pq -> pq p)) ep egpq);
    };

    fmap1Expression :: (Functor f1,Functor f2) =>
        (forall g. (Functor g) => f1 (g r1) -> f2 (g r2)) -> Expression wit f1 r1 -> Expression wit f2 r2;
    fmap1Expression f1f2 (ClosedExpression f1r1) = ClosedExpression (fmap runIdentity (f1f2 (fmap Identity f1r1)));
    fmap1Expression f1f2 (OpenExpression w egr1) = OpenExpression w (fmap1Expression ((fmap getCompose) . f1f2 . (fmap Compose)) egr1);

    expressionSymbol :: (Functor g) => f (g r) -> wit g -> Expression wit f r;
    expressionSymbol fgr wit = OpenExpression wit (ClosedExpression fgr);


    -- value expressions

    data Combine (combine :: * -> k -> k) (wit :: * -> *) (g :: k -> k) where
    {
        MkCombine :: wit val -> Combine combine wit (combine val);
    };

    type ValueExpression wit = Expression (Combine (->) wit);

    runValueExpression :: forall wit f r. (Functor f) => ValueExpression wit f r -> f ((forall val. wit val -> val) -> r);
    runValueExpression (ClosedExpression fr) = fmap (\r _ab -> r) fr;
    runValueExpression (OpenExpression (MkCombine (w :: wit val')) evr) = fmap ff (runValueExpression evr) where
    {
        ff :: ((forall val. wit val -> val) -> val' -> r) -> ((forall val. wit val -> val) -> r);
        ff abbr ab = abbr ab (ab w);
    };

    valueSymbol :: (Applicative f) => wit val -> ValueExpression wit f val;
    valueSymbol wit = expressionSymbol (pure id) (MkCombine wit);
    
    letBind :: (SimpleWitness wit,Applicative f) => wit val -> ValueExpression wit f val -> ValueExpression wit f r -> ValueExpression wit f r;
    letBind wit valexp (OpenExpression cw@(MkCombine wit') exp) = case matchWitness wit wit' of
    {
        Just MkEqualType -> exp <*> valexp;
        _ -> OpenExpression cw (letBind wit valexp exp);
    };
    letBind _wit _valexp exp = exp;


    -- pattern expressions

    type PatternExpression wit q = Expression (Combine (,) wit) (Compose ((->) q) Maybe);

    patternSymbol :: wit val -> PatternExpression wit val ();
    patternSymbol wit = expressionSymbol (Compose (\val -> pure (val,()))) (MkCombine wit);

    patternCofmap :: (a -> b) -> PatternExpression wit b r -> PatternExpression wit a r;
    patternCofmap ab (ClosedExpression (Compose bmv)) = ClosedExpression (Compose (bmv . ab));
    patternCofmap ab (OpenExpression w expr) = OpenExpression w (patternCofmap ab expr);

    pattern :: (q -> Maybe r) -> PatternExpression wit q r;
    pattern qmr = ClosedExpression (Compose qmr);
-}
    patternMatch :: (q -> Bool) -> PatternExpression wit q ();
    patternMatch qb = pattern (\q -> if qb q then Just () else Nothing);
    
    patternMatchEq :: (Eq q) => q -> PatternExpression wit q ();
    patternMatchEq q = patternMatch ((==) q);
{-
    patternBind :: (SimpleWitness wit,Functor f1,Functor f2) =>
        Expression (Combine (,) wit) f1 a ->
        Expression (Combine (->) wit) f2 b ->
        Expression (Combine (->) wit) (Compose f1 f2) (a,b);
    patternBind (ClosedExpression f1a) exp = fmap1Expression (\f2gb -> Compose (fmap (\a -> fmap (\gb -> fmap (\b -> (a,b)) gb) f2gb) f1a)) exp;
    patternBind (OpenExpression (MkCombine wit) expf1wa) exp =
    wit :: wit val
    expf1wa :: Expression Cp f1 (val,a)
    exp :: Expression Cv f2 b
    wanted :: Expression Cv (Compose f1 f2) (a,b);
-}
    -- monomorphic symbols, representing type val

    data MonoSymbol (sym :: *) (val :: *) (val' :: *) where
    {
        MkMonoSymbol :: sym -> MonoSymbol sym val val;
    };

    instance (Eq sym) => SimpleWitness (MonoSymbol sym val) where
    {
        matchWitness (MkMonoSymbol sym1) (MkMonoSymbol sym2) | sym1 == sym2 = Just MkEqualType;
        matchWitness _ _ = Nothing;
    };

    type MonoValueExpression sym val = ValueExpression (MonoSymbol sym val);
    type MonoPatternExpression sym val q = PatternExpression (MonoSymbol sym val) q;
    
    monoValueSymbol :: (Applicative f) => sym -> MonoValueExpression sym val f val;
    monoValueSymbol sym = valueSymbol (MkMonoSymbol sym);
   
    monoPatternSymbol :: sym -> MonoPatternExpression sym val val ();
    monoPatternSymbol sym = patternSymbol (MkMonoSymbol sym);
{-
    monoLetBind :: (Eq sym,Applicative f) => sym -> MonoValueExpression sym val f val -> MonoValueExpression sym val f r -> MonoValueExpression sym val f r;
    monoLetBind sym = letBind (MkMonoSymbol sym);
-}
    monoPatternBind :: (Eq sym,Applicative f) =>
        MonoPatternExpression sym val q () ->
        MonoValueExpression sym val f r ->
        MonoValueExpression sym val (Compose (Compose ((->) q) Maybe) f) r;
    monoPatternBind patExp valExp = matchBind patExp valExp;
    
{-
    matchBind :: (SimpleWitness wit,Functor f1,Functor f2) =>
        MatchExpression wit f1 a ->
        ValueExpression wit f2 b ->
        ValueExpression wit (Compose f1 f2) (a,b);

    type PatternExpression wit q = MatchExpression wit (Compose ((->) q) Maybe);
-}
    
{-    fmap (\r q -> (qmu q) >> (return r)) exp;
    monoPatternBind (OpenExpression sym patexp) (ClosedExpression fr) = ClosedExpression 

    ((sym,sym,sym,sym),q -> Maybe (val,val,val,val))
    ((sym,sym,sym,sym),f (val -> val -> val -> val -> r))
    ((sym),q -> Maybe (f (val -> r)))


    monoPatternBind (OpenExpression sym patexp) valexp = ...
    patexp :: MonoPatternExpression sym val q (val,())
    valexp :: MonoValueExpression sym val f r
-}    
    
    
{-    expressionSymbols :: MonoValueExpression sym val f r -> [sym];
    expressionSymbols (ClosedExpression _) = [];
    expressionSymbols (OpenExpression (MkCombine (MkMonoSymbol sym)) exp) = sym:(expressionSymbols exp);
    
    evalExpression :: MonoValueExpression sym val f r -> Either [sym] (f r);
    evalExpression (ClosedExpression fr) = Right fr;
    evalExpression exp = Left (expressionSymbols exp);
-}
}
