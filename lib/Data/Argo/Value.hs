module Data.Argo.Value where
{
    import Import;
    import qualified Data.ByteString as B;
    import Data.Argo.Number;
    import Data.Argo.SubValue;
    import Data.Argo.Read;
    
    data Value = NullValue | BoolValue Bool | NumberValue Number | StringValue String | ArrayValue [Value] | FunctionValue (Value -> Value) | ByteArrayValue ByteString | ActionValue (IO Value);
    
    castValue :: (ToValue Value a,FromValue Value b,?context :: String) => a -> b;
    castValue a = fromValue (toValue a :: Value);
    
    instance ToValue Value Value where
    {
        toValue = id;
    };
    
    instance FromValue Value Value where
    {
        fromValueMaybe = return;
    };
    
    instance ToValue Value () where
    {
        toValue () = NullValue;
    };
    
    instance FromValue Value () where
    {
        fromValueMaybe NullValue = return ();
        fromValueMaybe v = typeFail "null" v;
    };

    instance ToValue Value Bool where
    {
        toValue = BoolValue;
    };
    
    instance FromValue Value Bool where
    {
        fromValueMaybe (BoolValue x) = return x;
        fromValueMaybe v = typeFail "boolean" v;
    };

    instance ToValue Value Number where
    {
        toValue = NumberValue;
    };
    
    instance FromValue Value Number where
    {
        fromValueMaybe (NumberValue x) = return x;
        fromValueMaybe v = typeFail "number" v;
    };

    instance ToValue Value String where
    {
        toValue = StringValue;
    };
    
    instance FromValue Value String where
    {
        fromValueMaybe (StringValue x) = return x;
        fromValueMaybe v = typeFail "string" v;
    };

    instance ToValue Value ByteString where
    {
        toValue = ByteArrayValue;
    };
    
    instance FromValue Value ByteString where
    {
        fromValueMaybe (ByteArrayValue x) = return x;
        fromValueMaybe v = typeFail "bytes" v;
    };

    instance ToValue Value [Word8] where
    {
        toValue = toValue . B.pack;
    };
    
    instance FromValue Value [Word8] where
    {
        fromValueMaybe v = fmap B.unpack (fromValueMaybe v);
    };

    instance ToValue Value [Value] where
    {
        toValue x = ArrayValue (fmap toValue x);
    };
    
    instance FromValue Value [Value] where
    {
        fromValueMaybe (ArrayValue x) = return (fmap fromValue x);
        fromValueMaybe v = typeFail "array" v;
    };

    instance (FromValue Value a,ToValue Value b) => ToValue Value (a -> b) where
    {
        toValue ab = FunctionValue (toValue . ab . fromValue);
    };
    
    instance (ToValue Value a,FromValue Value b) => FromValue Value (a -> b) where
    {
        fromValueMaybe (FunctionValue x) = return (fromValue . x . toValue);
        fromValueMaybe v = typeFail "function" v;
    };

    instance (ToValue Value a) => ToValue Value (IO a) where
    {
        toValue x = ActionValue (fmap toValue x);
    };
    
    instance (FromValue Value a) => FromValue Value (IO a) where
    {
        fromValueMaybe (ActionValue x) = return (fmap fromValue x);
        fromValueMaybe v = typeFail "action" v;
    };
    
    instance Show Value where
    {
        show NullValue = "null";
        show (BoolValue True) = "true";
        show (BoolValue False) = "false";
        show (NumberValue n) = show n;
        show (StringValue s) = show s;
        show (ByteArrayValue bb) = "bytes \"" ++ (concat (fmap hexByte (B.unpack bb))) ++ "\"" where
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
