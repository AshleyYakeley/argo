module Main where
{
    import Data.List;
    import Data.Functor.Identity;
    import Data.Argo.Expression;
    import Data.Argo.Read;
    import Data.Argo.Value;

    main :: IO ();
    main = do
    {
        s <- getContents;
        exp <- readText s;
        case evalExpression exp of
        {
            Left syms -> do
            {
                fail ("undefined: " ++ (intercalate ", " syms));
            };
            Right (Identity (r :: Value)) -> do
            {
                putStrLn (show r);
            };
        };
    };
}
