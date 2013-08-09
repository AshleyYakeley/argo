module Main where
{
    import Data.List;
    import Data.Functor.Identity;
    import Data.Argo;

    main :: IO ();
    main = do
    {
        s <- getContents;
        expr <- readText s;
        case evalExpression expr of
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
