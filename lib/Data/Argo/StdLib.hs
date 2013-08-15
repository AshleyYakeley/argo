module Data.Argo.StdLib(stdLib,stdLibValue) where
{
    import Import;
    import Data.Argo.Read;
    import Data.Argo.Value;
    --import System.Process;

    fixV :: (Value -> Value) -> Value;
    fixV f = f (fixV f);

    defaultV :: Value -> Value -> Value;
    defaultV NullValue v = v;
    defaultV v _ = v;

    defaultFunction :: (Value -> Value) -> (Value -> Value) -> (Value -> Value);
    defaultFunction f1 f2 x = defaultV (f1 x) (f2 x);

    takeV :: Int -> Either [Value] String -> Either [Value] String;
    takeV i (Left arr) = Left (take i arr);
    takeV i (Right s) = Right (take i s);

    dropV :: Int -> Either [Value] String -> Either [Value] String;
    dropV i (Left arr) = Left (drop i arr);
    dropV i (Right s) = Right (drop i s);

    bytes :: String -> [Word8];
    bytes = digitStreamToBytes . toDigitStream where
    {
        toDigitStream :: String -> [Word8];
        toDigitStream [] = [];
        toDigitStream (c:cc) | (c >= '0') && (c <= '9') = (toEnum ((fromEnum c) - (fromEnum '0'))):(toDigitStream cc);
        toDigitStream (c:cc) | (c >= 'A') && (c <= 'F') = (toEnum ((fromEnum c) - (fromEnum 'A')) + 10):(toDigitStream cc);
        toDigitStream (c:cc) | (c >= 'a') && (c <= 'f') = (toEnum ((fromEnum c) - (fromEnum 'a')) + 10):(toDigitStream cc);
        toDigitStream (c:cc) | isSpace c = toDigitStream cc;
        toDigitStream (c:_) = error ("unrecognised char in bytes: " ++ [c]);
        
        digitStreamToBytes :: [Word8] -> [Word8];
        digitStreamToBytes [] = [];
        digitStreamToBytes [_] = error "extra hex digit";
        digitStreamToBytes (a:b:rest) = ((a * 16) + b):(digitStreamToBytes rest);
    };
    
     
{-
    source :: 

    fileRead :: String -> IO [Word8];

    fileWrite :: String -> [Word8] -> IO ();

    fileType :: String -> IO Value;
    fileType

    file :: String -> String -> Value;
    file path "type" = toValue (fileType path);
    file path "read" = toValue (fileRead path);
    file path "write" = toValue (fileWrite path);
    file _path _ = toValue ();

    startProcess :: (String -> Value) -> IO Int;
    startProcess fargs = let
    {
        arg s = fromValue (fargs s);
    };

    CreateProcess
    {
        cmdspec = RawCommand cmdpath args,
        cwd = arg "wd",
        env = Nothing,
        std_in = Inherit,
        std_out = Inherit,
        std_err = Inherit
    }

    process :: Int -> 
-}
    eq :: Value -> Value -> Bool;
    eq NullValue NullValue = True;
    eq (BoolValue a) (BoolValue b) = a == b;
    eq (NumberValue a) (NumberValue b) = a == b;
    eq (StringValue a) (StringValue b) = a == b;
    eq (ByteArrayValue a) (ByteArrayValue b) = a == b;
    eq (ArrayValue a) (ArrayValue b) = eqArray a b;
    eq _ _ = False;
    
    eqArray :: [Value] -> [Value] -> Bool;
    eqArray [] [] = True;
    eqArray (a:aa) (b:bb) | eq a b = eqArray aa bb;
    eqArray _ _ = False;

    stdLib :: String -> Value;
    stdLib "default" = toValue defaultV;
    stdLib "default-function" = toValue defaultFunction;
    stdLib "+" = toValue ((+) :: Rational -> Rational -> Rational);
    stdLib "-" = toValue ((-) :: Rational -> Rational -> Rational);
    stdLib "=" = toValue eq;

    stdLib "take" = toValue takeV;
    stdLib "drop" = toValue dropV;

    stdLib "bytes" = toValue bytes;

--    stdLib "file" = toValue file;

    stdLib ">>=" = toValue ((>>=) :: IO Value -> (Value -> IO Value) -> IO Value);
    stdLib "return" = toValue (return :: Value -> IO Value);
    stdLib "fix" = toValue fixV;
    stdLib _ = toValue ();

    stdLibValue :: Value;
    stdLibValue = toValue stdLib;
}
