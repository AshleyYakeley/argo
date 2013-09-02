{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Argo.StdLib.File(fileFunctions) where
{
    import Import;
    import Control.Exception;
    import System.FilePath;
    import System.Directory hiding (getCurrentDirectory,setCurrentDirectory);
    import System.Posix.Types;
    import System.Posix.Directory;
    import System.Posix.Files;
    import System.IO.UTF8;
    import Data.Argo.SubValue;
    import Data.Argo.Value;

    withWD :: FilePath -> IO Value -> IO Value;
    withWD path f = do
    {
        oldpath <- getWorkingDirectory;
        changeWorkingDirectory path;
        finally f (changeWorkingDirectory oldpath);
    };
    
    instance FromValue Value CUid where
    {
        fromValueMaybe v = fmap CUid (fromValueMaybe v);
    };
    
    instance ToValue Value CUid where
    {
        toValue (CUid uid) = toValue uid;
    };
    
    instance FromValue Value CGid where
    {
        fromValueMaybe v = fmap CGid (fromValueMaybe v);
    };
    
    instance ToValue Value CGid where
    {
        toValue (CGid uid) = toValue uid;
    };
    
    instance ToValue Value COff where
    {
        toValue (COff uid) = toValue uid;
    };
    
    fileType :: FileStatus -> String;
    fileType fs | isRegularFile fs = "file";
    fileType fs | isDirectory fs = "directory";
    fileType fs | isSymbolicLink fs = "slink";
    fileType fs | isNamedPipe fs = "pipe";
    fileType fs | isSocket fs = "socket";
    fileType fs | isBlockDevice fs = "block";
    fileType fs | isCharacterDevice fs = "char";
    fileType _ = "unknown";
    
    instance ToValue Value FileStatus where
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

    fileFunctions :: (?context :: String) => String -> Maybe Value;
    fileFunctions "path-combine" = Just (toValue combine);
    fileFunctions "path-rename" = Just (toValue rename);
    fileFunctions "path-touch" = Just (toValue touchFile);
    fileFunctions "path-setown" = Just (toValue setOwnerAndGroup);
    fileFunctions "path-status" = Just (toValue pathStatus);
    fileFunctions "path-type" = Just (toValue pathType);
    fileFunctions "file-get" = Just (toValue readFile);
    fileFunctions "file-set" = Just (toValue writeFile);
    fileFunctions "file-remove" = Just (toValue removeLink);
    fileFunctions "file-rename" = Just (toValue renameFile);
    fileFunctions "file-copy" = Just (toValue copyFile);
    fileFunctions "file-link" = Just (toValue createLink);
    fileFunctions "directory-create" = Just (toValue (createDirectoryIfMissing True));
    fileFunctions "directory-remove" = Just (toValue removeDirectoryRecursive);
    fileFunctions "directory-contents" = Just (toValue getDirectoryContents);
    fileFunctions "wd-get" = Just (toValue getWorkingDirectory);
    fileFunctions "wd-with" = Just (toValue withWD);
    fileFunctions "slink-create" = Just (toValue createSymbolicLink);
    fileFunctions "slink-get" = Just (toValue readSymbolicLink);
    fileFunctions "slink-setown" = Just (toValue setSymbolicLinkOwnerAndGroup);
    fileFunctions "slink-status" = Just (toValue getSymbolicLinkStatus);
    fileFunctions _ = Nothing;
}
