module Data.Argo.StdLib(stdLib,stdLibValue) where
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

    stdLib :: String -> Value;
    stdLib "default" = toValue defaultV;
    stdLib "default-function" = toValue defaultFunction;
    stdLib "+" = toValue ((+) :: Rational -> Rational -> Rational);
    stdLib "-" = toValue ((-) :: Rational -> Rational -> Rational);

    stdLib "take" = toValue takeV;
    stdLib "drop" = toValue dropV;

    stdLib ">>=" = toValue ((>>=) :: IO Value -> (Value -> IO Value) -> IO Value);
    stdLib "return" = toValue (return :: Value -> IO Value);
    stdLib "fix" = toValue fixV;
    stdLib _ = toValue ();

    stdLibValue :: Value;
    stdLibValue = toValue stdLib;
}
