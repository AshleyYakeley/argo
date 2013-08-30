module Data.Argo.Value where
{
    import Import;
    import Data.Argo.SubValue;
    import Data.Argo.Read;
    
    data Value = NullValue | BoolValue Bool | NumberValue Rational | StringValue String | ArrayValue [Value] | FunctionValue (Value -> Value) | ByteArrayValue [Word8] | ActionValue (IO Value);
    
    castValue :: (ToValue Value a,FromValue Value b) => a -> b;
    castValue a = fromValue (toValue a :: Value);
    
    instance ToValue Value Value where
    {
        toValue = id;
    };
    
    instance FromValue Value Value where
    {
        fromValueMaybe = Just;
    };
    
    instance ToValue Value () where
    {
        toValue () = NullValue;
    };
    
    instance FromValue Value () where
    {
        fromValueMaybe NullValue = Just ();
        fromValueMaybe _ = Nothing;
    };
    
    instance (ToValue Value a) => ToValue Value (Maybe a) where
    {
        toValue Nothing = NullValue;
        toValue (Just a) = toValue a;
    };
    
    instance (FromValue Value a) => FromValue Value (Maybe a) where
    {
        fromValueMaybe NullValue = Just Nothing;
        fromValueMaybe v = fmap Just (fromValueMaybe v);
    };

    instance ToValue Value Bool where
    {
        toValue = BoolValue;
    };
    
    instance FromValue Value Bool where
    {
        fromValueMaybe (BoolValue x) = Just x;
        fromValueMaybe _ = Nothing;
    };

    instance ToValue Value Rational where
    {
        toValue = NumberValue;
    };
    
    instance FromValue Value Rational where
    {
        fromValueMaybe (NumberValue x) = Just x;
        fromValueMaybe _ = Nothing;
    };

    instance ToValue Value Integer where
    {
        toValue x = toValue (fromInteger x :: Rational);
    };
    
    instance FromValue Value Integer where
    {
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

    instance ToValue Value Int where
    {
        toValue = toValue . toInteger;
    };
    
    instance FromValue Value Int where
    {
        fromValueMaybe v = fmap fromInteger (fromValueMaybe v :: Maybe Integer);
    };

    instance ToValue Value String where
    {
        toValue = StringValue;
    };
    
    instance FromValue Value String where
    {
        fromValueMaybe (StringValue x) = Just x;
        fromValueMaybe _ = Nothing;
    };

    instance ToValue Value [Word8] where
    {
        toValue = ByteArrayValue;
    };
    
    instance FromValue Value [Word8] where
    {
        fromValueMaybe (ByteArrayValue x) = Just x;
        fromValueMaybe _ = Nothing;
    };

    instance ToValue Value [String] where
    {
        toValue x = ArrayValue (fmap toValue x);
    };
    
    instance FromValue Value [String] where
    {
        fromValueMaybe (ArrayValue x) = Just (fmap fromValue x);
        fromValueMaybe _ = Nothing;
    };

    instance ({-ToValue Value a-}) => ToValue Value [Value] where
    {
        toValue x = ArrayValue (fmap toValue x);
    };
    
    instance ({-FromValue Value a-}) => FromValue Value [Value] where
    {
        fromValueMaybe (ArrayValue x) = Just (fmap fromValue x);
        fromValueMaybe _ = Nothing;
    };

    instance (FromValue Value a,ToValue Value b) => ToValue Value (a -> b) where
    {
        toValue ab = FunctionValue (\v -> case fromValueMaybe v of
        {
            Just a -> toValue (ab a);
            Nothing -> toValue ();
        });
    };
    
    instance (ToValue Value a,FromValue Value b) => FromValue Value (a -> b) where
    {
        fromValueMaybe (FunctionValue x) = Just (fromValue . x . toValue);
        fromValueMaybe _ = Nothing;
    };

    instance (ToValue Value a) => ToValue Value (IO a) where
    {
        toValue x = ActionValue (fmap toValue x);
    };
    
    instance (FromValue Value a) => FromValue Value (IO a) where
    {
        fromValueMaybe (ActionValue x) = Just (fmap fromValue x);
        fromValueMaybe _ = Nothing;
    };
    
    instance (ToValue Value a, ToValue Value b) => ToValue Value (Either a b) where
    {
        toValue (Left a) = toValue a;
        toValue (Right b) = toValue b;
    };
    
    instance (FromValue Value a, FromValue Value b) => FromValue Value (Either a b) where
    {
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
