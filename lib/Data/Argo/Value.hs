module Data.Argo.Value where
{
    import Import;
    import qualified Data.ByteString as B;
    import Data.Argo.Number;
    import Data.Argo.Record;
    
    data Value = 
        NullValue | 
        BoolValue Bool |
        NumberValue Number |
        StringValue String |
        ArrayValue [Value] | 
        RecordValue (Record Value) |
        FunctionValue (Value -> Value) |
        ByteArrayValue ByteString |
        ActionValue (IO Value);
    
    instance Show Value where
    {
        show NullValue = "null";
        show (BoolValue True) = "true";
        show (BoolValue False) = "false";
        show (NumberValue n) = showNumber n;
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
        show (RecordValue (MkRecord entries)) = "{" ++ showEntries entries ++ "}" where
        {
            showEntries [] = "";
            showEntries [(s,v)] = (show s) ++ ":" ++ (show v);
            showEntries ((s,v):as) = (show s) ++ ":" ++ (show v) ++ "," ++ (showEntries as);
        };
        show (FunctionValue _) = "{...}";
        show (ActionValue _) = "<action>";
    };
    
    valueTypeName :: Value -> String;
    valueTypeName NullValue = "null";
    valueTypeName (BoolValue _) = "boolean";
    valueTypeName (NumberValue _) = "number";
    valueTypeName (StringValue _) = "string";
    valueTypeName (ByteArrayValue _) = "bytes";
    valueTypeName (ArrayValue _) = "array";
    valueTypeName (RecordValue _) = "record";
    valueTypeName (FunctionValue _) = "function";
    valueTypeName (ActionValue _) = "action";
    
    castValue :: (ToValue a,FromValue b,?context :: String) => a -> b;
    castValue a = fromValue (toValue a :: Value);
    
    failC :: (Monad m,?context :: String) => String -> m a;
    failC s = fail (?context ++ ": " ++ s);
    
    errorC :: (?context :: String) => String -> a;
    errorC s = error (?context ++ ": " ++ s);
    
    class ToValue t where
    {
        toValue :: (?context :: String) => t -> Value;
        toValueList :: (?context :: String) => [t] -> Value;
        toValueList x = ArrayValue (fmap toValue x :: [Value]);
    };
    
    typeFail :: forall m a. (Monad m,?context :: String) => String -> Value -> m a;
    typeFail typeName v = failC ((show v) ++ " is not of type " ++ typeName);
    
    class FromValue t where
    {
        fromValueMaybe :: forall m. (Applicative m,Monad m,?context :: String) => Value -> m t;
        fromValueMaybeList :: forall m. (Applicative m,Monad m,?context :: String) => Value -> m [t];
        fromValueMaybeList (ArrayValue arr) = return (fmap fromValue arr);
        fromValueMaybeList v = typeFail "array" v;
    };

    isValue :: (Eq t,FromValue t,?context :: String) => t -> Value -> Bool;
    isValue t v = fromValueMaybe v == Just t;

    fromValue :: (FromValue t,?context :: String) => Value -> t;
    fromValue v = case fromValueMaybe v of
    {
        Right t -> t;
        Left err -> error err;
    };

    applyValue :: (?context :: String) => Value -> Value -> Value;
    applyValue = fromValue;
    
    type SubValue t = (FromValue t,ToValue t);


    -- Value
    
    instance ToValue Value where
    {
        toValue = id;
    };
    
    instance FromValue Value where
    {
        fromValueMaybe = return;
    };


    -- ()
    
    instance ToValue () where
    {
        toValue () = NullValue;
    };
    
    instance FromValue () where
    {
        fromValueMaybe NullValue = return ();
        fromValueMaybe v = typeFail "null" v;
    };


    -- Bool

    instance ToValue Bool where
    {
        toValue = BoolValue;
    };
    
    instance FromValue Bool where
    {
        fromValueMaybe (BoolValue x) = return x;
        fromValueMaybe v = typeFail "boolean" v;
    };


    -- Numbers

    instance ToValue Number where
    {
        toValue = NumberValue;
    };
    
    instance FromValue Number where
    {
        fromValueMaybe (NumberValue x) = return x;
        fromValueMaybe v = typeFail "number" v;
    };

    instance ToValue Integer where
    {
        toValue x = toValue (fromInteger x :: Rational);
    };
    
    instance FromValue Integer where
    {
        fromValueMaybe v = do
        {
            r :: Rational <- fromValueMaybe v;
            case denominator r of
            {
                1 -> return (numerator r);
                _ -> typeFail "integer" v;
            };
        };
    };

    instance ToValue Int where
    {
        toValue = toValue . toInteger;
    };
    
    instance FromValue Int where
    {
        fromValueMaybe v = fmap fromInteger (fromValueMaybe v);
    };

    instance ToValue Word32 where
    {
        toValue = toValue . toInteger;
    };
    
    instance FromValue Word32 where
    {
        fromValueMaybe v = fmap fromInteger (fromValueMaybe v);
    };

    instance ToValue Int32 where
    {
        toValue = toValue . toInteger;
    };
    
    instance FromValue Int32 where
    {
        fromValueMaybe v = fmap fromInteger (fromValueMaybe v);
    };

    instance ToValue Int64 where
    {
        toValue = toValue . toInteger;
    };
    
    instance FromValue Int64 where
    {
        fromValueMaybe v = fmap fromInteger (fromValueMaybe v);
    };

    instance ToValue CInt where
    {
        toValue (CInt x) = toValue x;
    };

    instance FromValue CInt where
    {
        fromValueMaybe v = fmap CInt (fromValueMaybe v);
    };


    -- Char & String
    
    instance ToValue Char where
    {
        toValue c = toValue [c];
        toValueList = StringValue;
    };
    
    instance FromValue Char where
    {
        fromValueMaybe (StringValue [x]) = return x;
        fromValueMaybe v = typeFail "char" v;
        fromValueMaybeList (StringValue x) = return x;
        fromValueMaybeList v = typeFail "string" v;
    };


    -- Word8 & ByteString

    instance ToValue ByteString where
    {
        toValue = ByteArrayValue;
    };
    
    instance FromValue ByteString where
    {
        fromValueMaybe (ByteArrayValue x) = return x;
        fromValueMaybe v = typeFail "bytes" v;
    };

    instance ToValue Word8 where
    {
        toValue x = toValue [x];
        toValueList = toValue . B.pack;
    };
    
    instance FromValue Word8 where
    {
        fromValueMaybe v = do
        {
            bb <- fromValueMaybe v;
            case bb of
            {
                [b] -> return b;
                _ -> typeFail "byte" v;
            };
        };

        fromValueMaybeList v = fmap B.unpack (fromValueMaybe v);
    };
    

    -- Array
    
    instance (ToValue t) => ToValue [t] where
    {
        toValue = toValueList;
    };
    
    instance (FromValue t) => FromValue [t] where
    {
        fromValueMaybe = fromValueMaybeList;
    };
    

    -- Record
    
    instance (ToValue t) => ToValue (Record t) where
    {
        toValue record = RecordValue (fmap toValue record);
    };
    
    instance (FromValue t) => FromValue (Record t) where
    {
        fromValueMaybe (RecordValue record) = traverse fromValueMaybe record;
        fromValueMaybe v = typeFail "record" v;
    };


    -- Function

    instance (FromValue a,ToValue b) => ToValue (a -> b) where
    {
        toValue ab = FunctionValue (toValue . ab . fromValue);
    };
    
    instance (ToValue a,FromValue b) => FromValue (a -> b) where
    {
        fromValueMaybe (FunctionValue x) = return (fromValue . x . toValue);
        fromValueMaybe v = typeFail "function" v;
    };


    -- Action

    instance (ToValue a) => ToValue (IO a) where
    {
        toValue x = ActionValue (fmap toValue x);
    };
    
    instance (FromValue a) => FromValue (IO a) where
    {
        fromValueMaybe (ActionValue x) = return (fmap fromValue x);
        fromValueMaybe v = typeFail "action" v;
    };


    -- Maybe
    
    instance (ToValue a) => ToValue (Maybe a) where
    {
        toValue Nothing = toValue ();
        toValue (Just a) = toValue a;
    };
    
    instance (FromValue a) => FromValue (Maybe a) where
    {
        fromValueMaybe v = case fromValueMaybe v of
        {
            Just a -> return (Just a);
            Nothing -> case fromValueMaybe v of
            {
                Just () -> return Nothing;
                Nothing -> typeFail "maybe" v;
            };
        };
    };
    
    
    -- Either
    
    instance (ToValue a, ToValue b) => ToValue (Either a b) where
    {
        toValue (Left a) = toValue a;
        toValue (Right b) = toValue b;
    };
    
    instance (FromValue a, FromValue b) => FromValue (Either a b) where
    {
        fromValueMaybe v = case fromValueMaybe v of
        {
            Just a -> return (Left a);
            Nothing -> case fromValueMaybe v of
            {
                Just b -> return (Right b);
                Nothing -> typeFail "either" v;
            };
        };
    };
    
    
    -- (,)
    
    instance (ToValue a, ToValue b) => ToValue (a,b) where
    {
        toValue (a,b) = toValue [toValue a,toValue b];
    };
    
    instance (FromValue a, FromValue b) => FromValue (a,b) where
    {
        fromValueMaybe v = do
        {
            vv <- fromValueMaybe v;
            case vv of
            {
                [va,vb] -> return (fromValue va,fromValue vb);
                _ -> typeFail "pair" v;
            };
        };
    };
}
