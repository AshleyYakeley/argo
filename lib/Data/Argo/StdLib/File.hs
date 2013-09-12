{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Argo.StdLib.File(fileFunctions) where
{
    import Import;
    import System.FilePath;
    import System.Directory hiding (getCurrentDirectory,setCurrentDirectory);
    import System.Posix.Files;
    import qualified Data.ByteString as B;
    import Data.Argo.Value;
    import Data.Argo.StdLib.Types();
    
    fileType :: FileStatus -> String;
    fileType fs | isRegularFile fs = "file";
    fileType fs | isDirectory fs = "directory";
    fileType fs | isSymbolicLink fs = "slink";
    fileType fs | isNamedPipe fs = "pipe";
    fileType fs | isSocket fs = "socket";
    fileType fs | isBlockDevice fs = "block";
    fileType fs | isCharacterDevice fs = "char";
    fileType _ = "unknown";
    
    instance ToValue FileStatus where
    {
        toValue fs = toValue fsf where
        {
            fsf :: String -> Value;
            fsf "owner" = toValue (fileOwner fs);
            fsf "group" = toValue (fileGroup fs);
            fsf "size" = toValue (fileSize fs);
            fsf "type" = toValue (fileType fs);
            fsf _ = toValue ();
        };
    };

    pathStatus :: FilePath -> IO (Maybe FileStatus);
    pathStatus path = do
    {
        ex <- fileExist path;
        if ex then do
        {
            st <- getFileStatus path;
            return (Just st);
        }
        else return Nothing;
    };

    pathType :: FilePath -> IO (Maybe String);
    pathType path = do
    {
        mstat <- pathStatus path;
        return (fmap fileType mstat);
    };

    pathConcat :: [FilePath] -> FilePath;
    pathConcat [p] = p;
    pathConcat (p:pp) = combine p (pathConcat pp);
    pathConcat [] = ".";

    fileFunctions :: (?context :: String) => String -> Maybe Value;
    fileFunctions "path-concat" = Just (toValue pathConcat);
    fileFunctions "path-split" = Just (toValue splitPath);
    fileFunctions "path-split-name" = Just (toValue splitFileName);
    fileFunctions "path-rename" = Just (toValue rename);
    fileFunctions "path-touch" = Just (toValue touchFile);
    fileFunctions "path-setown" = Just (toValue setOwnerAndGroup);
    fileFunctions "path-status" = Just (toValue pathStatus);
    fileFunctions "path-type" = Just (toValue pathType);
    fileFunctions "file-get" = Just (toValue B.readFile);
    fileFunctions "file-set" = Just (toValue B.writeFile);
    fileFunctions "file-append" = Just (toValue B.appendFile);
    fileFunctions "file-remove" = Just (toValue removeLink);
    fileFunctions "file-rename" = Just (toValue renameFile);
    fileFunctions "file-copy" = Just (toValue copyFile);
    fileFunctions "file-link" = Just (toValue createLink);
    fileFunctions "directory-create" = Just (toValue (createDirectoryIfMissing True));
    fileFunctions "directory-remove" = Just (toValue removeDirectoryRecursive);
    fileFunctions "directory-contents" = Just (toValue getDirectoryContents);
    fileFunctions "slink-create" = Just (toValue createSymbolicLink);
    fileFunctions "slink-get" = Just (toValue readSymbolicLink);
    fileFunctions "slink-setown" = Just (toValue setSymbolicLinkOwnerAndGroup);
    fileFunctions "slink-status" = Just (toValue getSymbolicLinkStatus);
    fileFunctions _ = Nothing;
}
