module Main where
{
    import Data.Argo;

    main :: IO ();
    main = do
    {
        s <- getContents;
        r :: Value <- evaluateWithLibs stdlib (\_ -> return Nothing) s;
        putStrLn (show r);
    };
}
