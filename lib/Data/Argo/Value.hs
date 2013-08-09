module Data.Argo.Value where
{
    import Import;
    import Data.Argo.Read;
    
    data Value = NullValue | BoolValue Bool | NumberValue Rational | StringValue String | ArrayValue [Value] | FunctionValue (Value -> Value);
    
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
        
        valueApply (FunctionValue f) x = f x;
        valueApply _ _ = error "non-function application";
    };
}
