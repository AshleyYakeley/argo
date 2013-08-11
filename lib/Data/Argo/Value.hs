module Data.Argo.Value where
{
    import Import;
    import Data.Argo.Read;
    
    data Value = NullValue | BoolValue Bool | NumberValue Rational | StringValue String | ArrayValue [Value] | FunctionValue (Value -> Value) | ActionValue (IO Value);
    
    instance SubValue Value Value where
    {
        toValue = id;
        fromValueMaybe = Just;
    };
    
    instance SubValue Value () where
    {
        toValue () = NullValue;
        fromValueMaybe NullValue = Just ();
        fromValueMaybe _ = Nothing;
    };

    instance SubValue Value Bool where
    {
        toValue = BoolValue;
        fromValueMaybe (BoolValue x) = Just x;
        fromValueMaybe _ = Nothing;
    };

    instance SubValue Value Rational where
    {
        toValue = NumberValue;
        fromValueMaybe (NumberValue x) = Just x;
        fromValueMaybe _ = Nothing;
    };

    instance SubValue Value String where
    {
        toValue = StringValue;
        fromValueMaybe (StringValue x) = Just x;
        fromValueMaybe _ = Nothing;
    };

    instance ({-SubValue Value a-}) => SubValue Value [Value] where
    {
        toValue x = ArrayValue (fmap toValue x);
        fromValueMaybe (ArrayValue x) = Just (fmap fromValue x);
        fromValueMaybe _ = Nothing;
    };

    instance (SubValue Value a,SubValue Value b) => SubValue Value (a -> b) where
    {
        toValue ab = FunctionValue (\v -> case fromValueMaybe v of
        {
            Just a -> toValue (ab a);
            Nothing -> toValue ();
        });
        fromValueMaybe (FunctionValue x) = Just (fromValue . x . toValue);
        fromValueMaybe _ = Nothing;
    };

    instance (SubValue Value a) => SubValue Value (IO a) where
    {
        toValue x = ActionValue (fmap toValue x);
        fromValueMaybe (ActionValue x) = Just (fmap fromValue x);
        fromValueMaybe _ = Nothing;
    };
    
    
    instance Show Value where
    {
        show NullValue = "null";
        show (BoolValue True) = "true";
        show (BoolValue False) = "false";
        show (NumberValue n) = show n;
        show (StringValue s) = show s;
        show (ArrayValue vs) = "[" ++ showVals vs ++ "]" where
        {
            showVals [] = "";
            showVals [a] = show a;
            showVals (a:as) = (show a) ++ "," ++ (showVals as);
        };
        show (FunctionValue _) = "{...}";
        show (ActionValue _) = "<action>";
    };
    
    instance ValueRead Value where
    {
        valueTypeName NullValue = "null";
        valueTypeName (BoolValue _) = "boolean";
        valueTypeName (NumberValue _) = "number";
        valueTypeName (StringValue _) = "string";
        valueTypeName (ArrayValue _) = "array";
        valueTypeName (FunctionValue _) = "function";
        valueTypeName (ActionValue _) = "function";
    };
}
