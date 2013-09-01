module Data.Argo.StdLib.Action(action) where
{
    import Import;
    import Data.Argo.SubValue;
    import Data.Argo.Value;

    data Actionable = ListActionable [Actionable] | FuncActionable (Value -> Actionable) | IOActionable (IO Value);
    
    instance FromValue Value [Actionable] where
    {
        fromValueMaybe (ArrayValue x) = return (fmap fromValue x);
        fromValueMaybe v = typeFail "array" v;
    };
    
    instance FromValue Value Actionable where
    {
        fromValueMaybe v = case fromValueMaybe v of
        {
            Just x -> return (ListActionable x);
            Nothing -> case fromValueMaybe v of
            {
                Just x -> return (FuncActionable x);
                Nothing -> case fromValueMaybe v of
                {
                    Just x -> return (IOActionable x);
                    Nothing -> typeFail "actionable" v;
                }
            }
        };
    };
    
    actionListOn :: Value -> [Actionable] -> IO Value;
    actionListOn v [] = return v;
    actionListOn v (a:aa) = do
    {
        r <- actionOn v a;
        actionListOn r aa;
    };
    
    actionOn :: Value -> Actionable -> IO Value;
    actionOn v (ListActionable list) = actionListOn v list;
    actionOn v (FuncActionable f) = action (f v);
    actionOn _v (IOActionable a) = a;
    
    action :: Actionable -> IO Value;
    action = actionOn NullValue;
}
