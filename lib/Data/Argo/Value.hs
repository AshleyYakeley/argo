module Data.Argo.Value where
{
    import Import;
    import Data.Argo.Read;
    
    data Value = NullValue | BoolValue Bool | NumberValue Rational | StringValue String | ArrayValue [Value] | FunctionValue (Value -> Value) | ByteArrayValue [Word8] | ActionValue (IO Value);
    
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
    
    instance (SubValue Value a) => SubValue Value (Maybe a) where
    {
        toValue Nothing = NullValue;
        toValue (Just a) = toValue a;
        fromValueMaybe NullValue = Just Nothing;
        fromValueMaybe v = fmap Just (fromValueMaybe v);
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

    instance SubValue Value Integer where
    {
        toValue x = toValue (fromInteger x :: Rational);
        fromValueMaybe v = do
        {
            r :: Rational <- fromValueMaybe v;
            case denominator r of
            {
                1 -> return (numerator r);
                _ -> Nothing;
            };
        };
    };

    instance SubValue Value Int where
    {
        toValue = toValue . toInteger;
        fromValueMaybe v = fmap fromInteger (fromValueMaybe v :: Maybe Integer);
    };

    instance SubValue Value String where
    {
        toValue = StringValue;
        fromValueMaybe (StringValue x) = Just x;
        fromValueMaybe _ = Nothing;
    };

    instance SubValue Value [Word8] where
    {
        toValue = ByteArrayValue;
        fromValueMaybe (ByteArrayValue x) = Just x;
        fromValueMaybe _ = Nothing;
    };

    instance SubValue Value [String] where
    {
        toValue x = ArrayValue (fmap toValue x);
        fromValueMaybe (ArrayValue x) = Just (fmap fromValue x);
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
    
    instance (SubValue Value a, SubValue Value b) => SubValue Value (Either a b) where
    {
        toValue (Left a) = toValue a;
        toValue (Right b) = toValue b;
        fromValueMaybe v = case fromValueMaybe v of
        {
            Just a -> Just (Left a);
            Nothing -> case fromValueMaybe v of
            {
                Just b -> Just (Right b);
                Nothing -> Nothing;
            };
        };
    };
    
    instance Show Value where
    {
        show NullValue = "null";
        show (BoolValue True) = "true";
        show (BoolValue False) = "false";
        show (NumberValue n) = show n;
        show (StringValue s) = show s;
        show (ByteArrayValue bb) = "bytes \"" ++ (concat (fmap hexByte bb)) ++ "\"" where
        {
            hexByte :: Word8 -> String;
            hexByte b = [hexChar (div b 16),hexChar (mod b 16)];
            
            hexChar :: Word8 -> Char;
            hexChar b | b < 10 = toEnum ((fromEnum '0') + (fromEnum b));
            hexChar b = toEnum ((fromEnum 'A') + (fromEnum b) - 10);
        };
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
        valueTypeName (ByteArrayValue _) = "bytes";
        valueTypeName (ArrayValue _) = "array";
        valueTypeName (FunctionValue _) = "function";
        valueTypeName (ActionValue _) = "action";
    };
}
