module Main where
{
    import Data.Functor.Identity;
    import Test.Framework;
    import TestUtil;
    import Data.Argo;

    parse :: String -> Maybe Value;
    parse s = do
    {
        expr <- readText s;
        case evalExpression expr of
        {
            Left _syms -> Nothing;
            Right (Identity (r :: Value)) -> return r;
        };
    };

    argoTest :: String -> Maybe Value -> Test;
    argoTest s mv = pureTest s (diff (show mv) (show (parse s)));

    tests :: [Test];
    tests = 
    [
        argoTest "3" (Just (NumberValue 3)),
        argoTest "{a:a}" (Just (FunctionValue id)),
        argoTest "{a:a} 45" (Just (NumberValue 45)),
        argoTest "{a:12} 45" (Just (NumberValue 12)),
        argoTest "\"\"" (Just (StringValue "")),
        argoTest "\"hx\"" (Just (StringValue "hx"))
    ];

    main :: IO ();
    main = defaultMain tests;
}
