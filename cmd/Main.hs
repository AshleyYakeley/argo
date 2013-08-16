module Main where
{
    import Prelude hiding (getContents, readFile);
    import Data.Argo;
    import System.Environment;
    import System.FilePath;
    import System.IO.UTF8;
    import System.Directory;

    interpretArgs :: [String] -> ([FilePath],Maybe FilePath,[String]);
    interpretArgs ("-I":dir:rest) = case interpretArgs rest of
    {
        (dirs,mFilePath,appArgs) -> (dir:dirs,mFilePath,appArgs);
    };
    interpretArgs ("-":rest) = ([],Nothing,rest);
    interpretArgs (s:rest) = ([],Just s,rest);
    interpretArgs [] = ([],Nothing,[]);

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

    lookupArgoFile :: [FilePath] -> FilePath -> IO (Maybe String);
    lookupArgoFile [] _ = return Nothing;
    lookupArgoFile (dir:dd) name = do
    {
        mr <- readFileIfExists (combine dir (addExtension name "argo"));
        case mr of
        {
            Just r -> return (Just r);
            Nothing -> lookupArgoFile dd name;
        };
    };

    main :: IO ();
    main = do
    {
        args <- getArgs;
        let
        {
            (dirs,mFilePath,appArgs) = interpretArgs args;
        };
        s <- case mFilePath of
        {
            Nothing -> getContents;
            Just filePath -> readFile filePath;
        };
        r :: Value <- evaluateWithLibs stdLibValue (lookupArgoFile dirs) s;
        _ :: Value <- fromValue (applyValue r (toValue appArgs));
        return ();
    };
}
