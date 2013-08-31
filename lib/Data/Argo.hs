module Data.Argo(module Data.Argo) where
{
    import Data.Argo.SubValue as Data.Argo;
    import Data.Argo.Read as Data.Argo;
    import Data.Argo.Value as Data.Argo;
    import Data.Argo.StdLib as Data.Argo;
    
    import Import;
    import System.IO.UTF8;
    import System.FilePath;
    import System.Directory;

    evaluateWithStdLib :: (Applicative m, MonadFix m,FromValue Value a) => (String -> m (Maybe String)) -> String -> m a;
    evaluateWithStdLib libReader source = do
    {
        val :: Value <- evaluateWithLibs stdLibValue libReader source;
        fromValueM val;
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

    evaluateWithDirs :: (FromValue Value a) => [FilePath] -> String -> IO a;
    evaluateWithDirs dirs = evaluateWithStdLib (lookupArgoFileInDirs dirs);
}
