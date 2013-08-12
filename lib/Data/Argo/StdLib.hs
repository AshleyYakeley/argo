module Data.Argo.StdLib(stdlib) where
{
    import Import;
    import Data.Argo.Read;
    import Data.Argo.Value;

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

    lib :: String -> Value;
    lib "default" = toValue defaultV;
    lib "default-function" = toValue defaultFunction;
    lib "+" = toValue ((+) :: Rational -> Rational -> Rational);
    lib "-" = toValue ((-) :: Rational -> Rational -> Rational);

    lib "take" = toValue takeV;
    lib "drop" = toValue dropV;

    lib ">>=" = toValue ((>>=) :: IO Value -> (Value -> IO Value) -> IO Value);
    lib "return" = toValue (return :: Value -> IO Value);
    lib "fix" = toValue fixV;
    lib _ = toValue ();

    stdlib :: Value;
    stdlib = toValue lib;
}
