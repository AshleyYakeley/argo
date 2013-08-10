module Data.Argo.Expression where
{
    import Import;
    import GHC.Exts(Constraint);

    data ConstraintWitness (constraint :: Constraint) where
    {
        MkConstraintWitness :: constraint => ConstraintWitness constraint;
    };

    listTypeToList :: (forall a. w a -> r) -> ListType w t -> [r];
    listTypeToList _wr NilListType = [];
    listTypeToList wr (ConsListType wa rest) = (wr wa):(listTypeToList wr rest);

    listTypeMap :: (forall a. w1 a -> w2 a) -> ListType w1 t -> ListType w2 t;
    listTypeMap _ww NilListType = NilListType;
    listTypeMap ww (ConsListType wa rest) = ConsListType (ww wa) (listTypeMap ww rest);

    listIdentity :: ListType Identity t -> t;
    listIdentity NilListType = ();
    listIdentity (ConsListType (Identity a) rest) = (a,listIdentity rest);

    listSequence ::  (Applicative f) => ListType f t -> f t;
    listSequence NilListType = pure ();
    listSequence (ConsListType fa rest) = liftA2 (,) fa (listSequence rest);

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
    witnessedListAppend NilListType _ = MkConstraintWitness;
    witnessedListAppend (ConsListType _ wla) wlb = case witnessedListAppend wla wlb of
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

    newtype EitherWitness w1 w2 a = MkEitherWitness (Either (w1 a) (w2 a));

    data PartitionList wit1 wit2 l = forall l1 l2. MkPartitionList
    {
        listPartitionWitness1 :: ListType wit1 l1,
        listPartitionWitness2 :: ListType wit2 l2,
        listFromPartition :: l1 -> l2 -> l,
        listToPartition1 :: l -> l1,
        listToPartition2 :: l -> l2
    };

    partitionList :: ListType (EitherWitness w1 w2) l -> PartitionList w1 w2 l;
    partitionList NilListType = MkPartitionList
    {
        listPartitionWitness1 = NilListType,
        listPartitionWitness2 = NilListType,
        listFromPartition = \() () -> (),
        listToPartition1 = \() -> (),
        listToPartition2 = \() -> ()
    };
    partitionList (ConsListType (MkEitherWitness (Left w1a)) rest) = case partitionList rest of
    {
        MkPartitionList pw1 pw2 fp tp1 tp2 -> MkPartitionList
        {
            listPartitionWitness1 = ConsListType w1a pw1,
            listPartitionWitness2 = pw2,
            listFromPartition = \(a,l1) l2 -> (a,fp l1 l2),
            listToPartition1 = \(a,l) -> (a,tp1 l),
            listToPartition2 = \(_,l) -> tp2 l
        };
    };
    partitionList (ConsListType (MkEitherWitness (Right w2a)) rest) = case partitionList rest of
    {
        MkPartitionList pw1 pw2 fp tp1 tp2 -> MkPartitionList
        {
            listPartitionWitness1 = pw1,
            listPartitionWitness2 = ConsListType w2a pw2,
            listFromPartition = \l1 (a,l2) -> (a,fp l1 l2),
            listToPartition1 = \(_,l) -> tp1 l,
            listToPartition2 = \(a,l) -> (a,tp2 l)
        };
    };




    data Expression (combine :: * -> * -> *) (wit :: * -> *) (f :: * -> *) (r :: *) where
    {
        MkExpression :: ListType wit vals -> f (combine vals r) -> Expression combine wit f r;
    };

    expressionSymbol :: f (combine (val,()) r) -> wit val -> Expression combine wit f r;
    expressionSymbol fcvr wit = MkExpression (ConsListType wit NilListType) fcvr;

    witMap :: (forall a. wit1 a -> wit2 a) -> Expression combine wit1 f r -> Expression combine wit2 f r;
    witMap ww (MkExpression wits fcvr) = MkExpression (listTypeMap ww wits) fcvr;

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
        MkRemoveFromList newwits ins _rem -> MkExpression newwits (fmap (\vsr vals a -> vsr (ins a vals)) fvsr);
    };

    reduceSymbols :: (Functor f) => (forall a. witP a -> Either (witQ a) a) -> ValueExpression witP f r -> ValueExpression witQ f r;
    reduceSymbols wm (MkExpression wits fvsr) =
     case partitionList (listTypeMap (\witP -> MkEitherWitness (fmap Identity (wm witP))) wits) of
    {
        MkPartitionList pw1 pw2 fp _tp1 _tp2 -> MkExpression pw1 (fmap (\vpr vq -> vpr (fp vq (listIdentity pw2))) fvsr);
    };

    letBind :: (SimpleWitness wit,Applicative f) => wit val -> ValueExpression wit f val -> ValueExpression wit f r -> ValueExpression wit f r;
    letBind wit valExp exp = (abstract wit exp) <*> valExp;

    evaluateExpression :: (Applicative m,Functor f) => (forall val. wit val -> m val) -> ValueExpression wit f r -> m (f r);
    evaluateExpression resolve (MkExpression wits fvr) =
     fmap (\vals -> fmap (\vr -> vr vals) fvr) (listSequence (listTypeMap resolve wits));

    matchBind :: (SimpleWitness wit,Functor f1,Functor f2) =>
        MatchExpression wit f1 a ->
        ValueExpression wit f2 b ->
        ValueExpression wit (Compose f1 f2) (a,b);
    matchBind (MkExpression matchWits f1vca) (MkExpression valueWits f2vtb) = case removeAllMatchingMany matchWits valueWits of
    {
        MkRemoveManyFromList newValueWits insM _remM -> MkExpression newValueWits
         (Compose (fmap (\(lx,a) -> fmap (\lb lr -> (a,lb (insM lx lr))) f2vtb) f1vca));
    };

    matchSimple :: (Functor f) => f r -> MatchExpression wit f r;
    matchSimple fr = MkExpression NilListType (fmap (\r -> ((),r)) fr);

    matchBoth :: (Applicative f) => MatchExpression wit f () -> MatchExpression wit f () -> MatchExpression wit f ();
    matchBoth = liftA2 (\_ _ -> ());

    matchAll :: (Applicative f) => [MatchExpression wit f ()] -> MatchExpression wit f ();
    matchAll [] = pure ();
    matchAll (exp:exps) = matchBoth exp (matchAll exps);

    toSimpleValueExpression :: (Functor f) =>
     ValueExpression wit f r -> ValueExpression wit Identity (f r);
    toSimpleValueExpression (MkExpression vwits fvsr) = MkExpression vwits (Identity (\vals -> fmap (\vsr -> vsr vals) fvsr));


    type PatternExpression wit q = MatchExpression wit (Compose ((->) q) Maybe);

    patternSymbol :: wit val -> PatternExpression wit val ();
    patternSymbol = expressionSymbol (Compose (\val -> pure ((val,()),())));

    pattern :: (q -> Maybe r) -> PatternExpression wit q r;
    pattern qmr = matchSimple (Compose qmr);

    patternMatch :: (q -> Bool) -> PatternExpression wit q ();
    patternMatch qb = pattern (\q -> if qb q then Just () else Nothing);
    
    patternMatchEq :: (Eq q) => q -> PatternExpression wit q ();
    patternMatchEq q = patternMatch ((==) q);
    
    composePattern :: PatternExpression wit q r -> PatternExpression wit p q -> PatternExpression wit p r;
    composePattern (MkExpression wits1 (Compose qmv1r)) (MkExpression wits2 (Compose pmv2q)) =
     case witnessedListAppend wits1 wits2 of
    {
        MkConstraintWitness -> MkExpression (listAppendWitness wits1 wits2) (Compose (\p -> do
        {
            (vals2,q) <- pmv2q p;
            (vals1,r) <- qmv1r q;
            return (listJoin vals1 vals2,r);
        }));
    };
    
    subPattern :: (q -> Maybe p) -> PatternExpression wit p r -> PatternExpression wit q r;
    subPattern qmp patp = composePattern patp (pattern qmp);
    
    patternMatchPair :: PatternExpression wit p () -> PatternExpression wit q () -> PatternExpression wit (p,q) ();
    patternMatchPair patp patq = matchBoth (subPattern (Just . fst) patp) (subPattern (Just . snd) patq);

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

    monoWitMap :: (sym1 -> sym2) ->
     Expression combine (MonoSymbol sym1 val) f r -> Expression combine (MonoSymbol sym2 val) f r;
    monoWitMap ss = witMap (\(MkMonoSymbol sym1) -> MkMonoSymbol (ss sym1));
    
    monoValueSymbol :: (Applicative f) => sym -> MonoValueExpression sym val f val;
    monoValueSymbol sym = valueSymbol (MkMonoSymbol sym);
   
    monoPatternSymbol :: sym -> MonoPatternExpression sym val val ();
    monoPatternSymbol sym = patternSymbol (MkMonoSymbol sym);


    monoLetBind :: (SimpleWitness wit,Eq sym,Applicative f) =>
     sym -> MonoValueExpression sym val f val -> MonoValueExpression sym val f r -> MonoValueExpression sym val f r;
    monoLetBind sym = letBind (MkMonoSymbol sym);

    monoPatternBind :: (Eq sym,Applicative f) =>
        MonoPatternExpression sym val q () ->
        MonoValueExpression sym val f r ->
        MonoValueExpression sym val (Compose (Compose ((->) q) Maybe) f) r;
    monoPatternBind patExp valExp = fmap snd (matchBind patExp valExp);
    
    expressionSymbols :: MonoValueExpression sym val f r -> [sym];
    expressionSymbols (MkExpression symlist _) = listTypeToList (\(MkMonoSymbol sym) -> sym) symlist;
    
    monoReduceSymbols :: (Functor f) =>
     (sym1 -> Either sym2 val) -> MonoValueExpression sym1 val f r -> MonoValueExpression sym2 val f r;
    monoReduceSymbols ss = reduceSymbols (\(MkMonoSymbol sym1) -> case ss sym1 of
    {
        Left sym2 -> Left (MkMonoSymbol sym2);
        Right val -> Right val;
    });

    monoEvaluateExpression :: (Applicative m,Functor f) => (sym -> m val) -> MonoValueExpression sym val f r -> m (f r);
    monoEvaluateExpression smv = evaluateExpression (\(MkMonoSymbol sym) -> smv sym);
    
    evalExpression :: (Functor f) => MonoValueExpression sym val f r -> Either [sym] (f r);
    evalExpression (MkExpression NilListType fnr) = Right (fmap (\nr -> nr ()) fnr);
    evalExpression exp = Left (expressionSymbols exp);
}
