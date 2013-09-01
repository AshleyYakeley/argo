module Data.Argo.StdLib(stdLib,stdLibValue) where
{
    import Import;
    import Data.Argo.SubValue;
    import Data.Argo.Value;
    import Data.Argo.StdLib.Action;
--    import Data.Argo.StdLib.File;
    import Data.Argo.StdLib.Process;
    import System.IO.UTF8;
    import System.IO hiding (hPutStr);

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
    
    subst :: String -> String -> String -> (String -> String) -> String;
    subst pre post template dict = case searchList pre template of
    {
        Just (a,aa) -> case searchList post aa of
        {
            Just (b,c) -> a ++ (dict b) ++ (subst pre post c dict);
            Nothing -> template;
        };
        Nothing -> template;
    }
    where
    {
        -- searchList s (a ++ s ++ b) = Just (a,b) where s is first occurrence
        searchList :: (Eq a) => [a] -> [a] -> Maybe ([a],[a]);
        searchList [] text = Just ([],text);
        searchList _ [] = Nothing;
        searchList (s:ss) (t:tt) | s == t = searchList ss tt;
        searchList sc (t:tt) = do
        {
            (a,b) <- searchList sc tt;
            return (t:a,b);
        };
    };

    stdoutV :: String -> IO ();
    stdoutV = hPutStr stdout;

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

    stdLib :: (?context :: String) => String -> Value;
    stdLib "default" = toValue defaultV;
    stdLib "default-function" = toValue defaultFunction;
    stdLib "+" = toValue ((+) :: Rational -> Rational -> Rational);
    stdLib "-" = toValue ((-) :: Rational -> Rational -> Rational);
    stdLib "=" = toValue eq;

    stdLib "take" = toValue takeV;
    stdLib "drop" = toValue dropV;
    stdLib "subst" = toValue subst;

    stdLib "bytes" = toValue bytes;

--    stdLib "file" = toValue file;

    stdLib "start-process" = toValue startProcess;
    stdLib "run-process" = toValue runProcess;

    stdLib "stdout" = toValue stdoutV;

    stdLib ">>=" = toValue ((>>=) :: IO Value -> (Value -> IO Value) -> IO Value);
    stdLib "return" = toValue (return :: Value -> IO Value);
    stdLib "action" = toValue action;
    stdLib "fix" = toValue fixV;
    stdLib _ = toValue ();

    stdLibValue :: (?context :: String) => Value;
    stdLibValue = toValue stdLib;
}
