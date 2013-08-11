module Data.Argo.Value where
{
    import Import;
    import Data.Argo.Read;
    
    data Value = NullValue | BoolValue Bool | NumberValue Rational | StringValue String | ArrayValue [Value] | FunctionValue (Value -> Value);
    
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
    };
    
    instance ValueRead Value where
    {
        valueNull = NullValue;
        valueConstant "null" = Just NullValue;
        valueConstant "true" = Just (BoolValue True);
        valueConstant "false" = Just (BoolValue False);
        valueConstant _ = Nothing;

        valueIsConstant "null" = Just (\v -> case v of
        {
            NullValue -> True;
            _ -> False;
        });
        valueIsConstant "true" = Just (\v -> case v of
        {
            BoolValue True -> True;
            _ -> False;
        });
        valueIsConstant "false" = Just (\v -> case v of
        {
            BoolValue False -> True;
            _ -> False;
        });
        valueIsConstant _ = Nothing;

        valueFromString = StringValue;

        valueIsString s (StringValue s') = s == s';
        valueIsString _ _ = False;

        valueFromNumber = NumberValue;

        valueIsNumber n (NumberValue n') = n == n';
        valueIsNumber _ _ = False;

        valueFromArray = ArrayValue;

        valueIsArray (ArrayValue arr) = Just arr;
        valueIsArray _ = Nothing;

        valueFromFunction = FunctionValue;

        valueIsFunction (FunctionValue f) = Just f;
        valueIsFunction _ = Nothing;

        valueIsType "null" NullValue = True;
        valueIsType "boolean" (BoolValue _) = True;
        valueIsType "number" (NumberValue _) = True;
        valueIsType "string" (StringValue _) = True;
        valueIsType "array" (ArrayValue _) = True;
        valueIsType "function" (FunctionValue _) = True;
        valueIsType _ _ = False;
    };
}
