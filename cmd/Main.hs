module Main where
{
    import Data.Argo;

    main :: IO ();
    main = do
    {
        s <- getContents;
        r :: Value <- evaluate (\_ -> return Nothing) s;
        putStrLn (show r);
    };
}
