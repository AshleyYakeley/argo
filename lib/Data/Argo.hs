module Data.Argo(module Data.Argo) where
{
    import Data.Argo.Number as Data.Argo;
    import Data.Argo.Value as Data.Argo;
    import Data.Argo.Read as Data.Argo;
    import Data.Argo.StdLib as Data.Argo;
    
    import Import;
    import System.IO.UTF8;
    import System.FilePath;
    import System.Directory;

    evaluateWithStdLib :: (Applicative m, MonadFix m,FromValue a,?context :: String) => String -> (String -> m (Maybe String)) -> String -> m a;
    evaluateWithStdLib sourcename libReader source = do
    {
        val :: Value <- evaluateWithLibs sourcename (\s -> case s of
        {
            "std" -> return (Just (Right stdLibValue));
            _ -> fmap (fmap Left) (libReader s);
        }) source;
        fromValueMaybe val;
    };

    readFileIfExists :: FilePath -> IO (Maybe String);
    readFileIfExists path = do
    {
        exists <- doesFileExist path;
        if exists then do
        {
            contents <- readFile path;
            return (Just contents);
        } else return Nothing;
    };

    lookupArgoFileInDirs :: [FilePath] -> FilePath -> IO (Maybe String);
    lookupArgoFileInDirs [] _ = return Nothing;
    lookupArgoFileInDirs (dir:dd) name = do
    {
        mr <- readFileIfExists (combine dir (addExtension name "argo"));
        case mr of
        {
            Just r -> return (Just r);
            Nothing -> lookupArgoFileInDirs dd name;
        };
    };

    evaluateWithDirs :: (FromValue a,?context :: String) => String -> [FilePath] -> String -> IO a;
    evaluateWithDirs sourcename dirs = evaluateWithStdLib sourcename (lookupArgoFileInDirs dirs);
}
