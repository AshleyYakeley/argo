module Data.Argo.MonoExpression where
{
    import Import;
    import Data.Argo.Expression;

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
    type MonoPatternExpression sym val ff q = PatternExpression (MonoSymbol sym val) ff q;

    monoWitMap :: (sym1 -> sym2) ->
     Expression combine (MonoSymbol sym1 val) f r -> Expression combine (MonoSymbol sym2 val) f r;
    monoWitMap ss = witMap (\(MkMonoSymbol sym1) -> MkMonoSymbol (ss sym1));

    monoValueSymbol :: (Applicative f) => sym -> MonoValueExpression sym val f val;
    monoValueSymbol sym = valueSymbol (MkMonoSymbol sym);

    monoPatternSymbol :: (Applicative ff) => sym -> MonoPatternExpression sym val ff val ();
    monoPatternSymbol sym = patternSymbol (MkMonoSymbol sym);

    monoLetBind :: (SimpleWitness wit,Eq sym,Applicative f) =>
     sym -> MonoValueExpression sym val f val -> MonoValueExpression sym val f r -> MonoValueExpression sym val f r;
    monoLetBind sym = letBind (MkMonoSymbol sym);

    monoPatternBind :: (Eq sym,Applicative f,Functor ff) =>
        MonoPatternExpression sym val ff q () ->
        MonoValueExpression sym val f r ->
        MonoValueExpression sym val (Compose (Compose ((->) q) ff) f) r;
    monoPatternBind patExp valExp = fmap snd (matchBind patExp valExp);

    monoEvaluateExpression :: (Applicative m,Functor f) => (sym -> m val) -> MonoValueExpression sym val f r -> m (f r);
    monoEvaluateExpression smv = evaluateExpression (\(MkMonoSymbol sym) -> smv sym);
}
