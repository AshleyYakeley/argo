{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Argo.StdLib.Process(processFunctions) where
{
    import Import;
    import Control.Exception;
    import System.Exit;
    import System.IO.Error;
    import System.Posix.Types;
    import System.Posix.Env;
    import System.Posix.Directory;
    import System.Posix.Process;
    import System.Posix.Signals;
    import System.Posix.User;
    import Data.Argo.Value;
    import Data.Argo.StdLib.Types();


    instance ToValue ExitCode where
    {
        toValue ExitSuccess = toValue ();
        toValue (ExitFailure code) = toValue code;
    };

    processStatus :: (?context :: String) => ProcessStatus -> Maybe String -> Value;
    processStatus (Exited _) Nothing = toValue "exited";
    processStatus (Exited code) (Just "error") = toValue code;
    processStatus (Terminated _) Nothing = toValue "terminated";
    processStatus (Terminated signal) (Just "signal") = toValue signal;
    processStatus (Stopped _) Nothing = toValue "stopped";
    processStatus (Stopped signal) (Just "signal") = toValue signal;
    processStatus _ (Just _) = toValue ();

    failProcess :: ProcessStatus -> IO ();
    failProcess (Exited ExitSuccess) = return ();
    failProcess p = fail ("process: " ++ (show p));

    instance ToValue ProcessStatus where
    {
        toValue ps = toValue (processStatus ps);
    };

    waitProcess :: (?context :: String) => ProcessID -> IO ProcessStatus;
    waitProcess pid = do
    {
        mps <- getProcessStatus True False pid;
        case mps of
        {
            Just ps -> return ps;
            Nothing -> failC "process still running after wait";
        };
    };

    processID :: (?context :: String) => ProcessID -> String -> Value;
    processID (CPid pid) "id" = toValue pid;
    processID pid "check" = toValue (getProcessStatus False True pid);
    processID pid "wait" = toValue (waitProcess pid);
    processID pid "signal" = toValue (\signal -> signalProcess signal pid);
    processID pid "terminate" = toValue (signalProcess softwareTermination pid);
    processID pid "kill" = toValue (signalProcess killProcess pid);
    processID _ _ = toValue ();

    instance ToValue ProcessID where
    {
        toValue pid = toValue (processID pid);
    };

    startProcess :: (?context :: String) => (Maybe String -> Value) -> IO ProcessID;
    startProcess fargs = let
    {
        cmdpath = fromValue (fargs Nothing);
        args = case fromValue (fargs (Just "args")) of
        {
            Just a -> a;
            Nothing -> [];
        };
        env = Nothing;
    } in do
    {
        forkProcess (do
        {
            setContext (fargs . Just);
            executeFile cmdpath False args env;
        });
    };
    
    runProcess :: (?context :: String) => (Maybe String -> Value) -> IO ();
    runProcess fargs = do
    {
        pid <- startProcess fargs;
        ps <- waitProcess pid;
        failProcess ps;
    };
    
    getContext :: (?context::String) => IO (String -> Maybe Value);
    getContext = do
    {
        ruid <- getRealUserID;
--        rusername <- getLoginName;
        rgid <- getRealGroupID;
        euid <- getEffectiveUserID;
--        eusername <- getEffectiveUserName;
        egid <- getEffectiveGroupID;
        groups <- getGroups;
        env <- getEnvironment;
        wd <- getWorkingDirectory;
        return (\s -> case s of
        {
            "real-user" -> Just (toValue ruid);
--            "real-username" -> Just (toValue rusername);
            "real-group" -> Just (toValue rgid);
            "user" -> Just (toValue euid);
--            "username" -> Just (toValue eusername);
            "group" -> Just (toValue egid);
            "groups" -> Just (toValue groups);
            "environment" -> Just (toValue env);
            "wd" -> Just (toValue wd);
            _ -> Nothing;
        });
    };

    setContext :: (?context :: String) => (String -> Value) -> IO ();
    setContext args = do
    {
        case fromValue (args "user") of
        {
            Just uid -> setEffectiveUserID uid;
            Nothing -> return ();
        };
        case fromValue (args "group") of
        {
            Just gid -> setEffectiveGroupID gid;
            Nothing -> return ();
        };
        case fromValue (args "environment") of
        {
            Just env -> setEnvironment env;
            Nothing -> return ();
        };
        case fromValue (args "wd") of
        {
            Just wd -> changeWorkingDirectory wd;
            Nothing -> return ();
        };
    };

    withContext :: (?context :: String) =>
     (String -> Value) -> IO Value -> IO Value;
    withContext args f0 = let
    {
        f1 = withThing getEnvironment setEnvironment (fromValue (args "environment")) f0;
        -- low-privilege user must be "inside" group and WD
        f2 = withThingCheck getEffectiveUserID setEffectiveUserID (fromValue (args "user")) f1;
        f3 = withThingCheck getEffectiveGroupID setEffectiveGroupID (fromValue (args "group")) f2;
        f4 = withThingCheck getWorkingDirectory changeWorkingDirectory (fromValue (args "wd")) f3;
    } in f4;

    withThing :: IO a -> (a -> IO ()) -> Maybe a -> IO Value -> IO Value;
    withThing _getter _setter Nothing f = f;
    withThing getter setter (Just thing) f = do
    {
        oldthing <- getter;
        setter thing;
        finally f (setter oldthing);
    };

    withThingCheck :: (Eq a) => IO a -> (a -> IO ()) -> Maybe a -> IO Value -> IO Value;
    withThingCheck _getter _setter Nothing f = f;
    withThingCheck getter setter (Just thing) f = do
    {
        oldthing <- getter;
        if thing == oldthing then f
        else do
        {
            setter thing;
            finally f (setter oldthing);
        };
    };

    instance ToValue UserEntry where
    {
        toValue ue = toValue (\s -> case s of
        {
            "name" -> toValue (userName ue);
            "password" -> toValue (userPassword ue);
            "id" -> toValue (userID ue);
            "group" -> toValue (userGroupID ue);
            "gecos" -> toValue (userGecos ue);
            "home" -> toValue (homeDirectory ue);
            "shell" -> toValue (userShell ue);
            _ -> errorC ("not in userentry: " ++ s);
        });
    };

    checkUserEntry :: IO UserEntry -> IO (Maybe UserEntry);
    checkUserEntry f = catch (do
    {
        ue <- f;
        return (Just ue);
    }) (\(ex :: IOError) -> if ioeGetErrorType ex == doesNotExistErrorType
     then return Nothing
     else throw ex);

    getUserEntryForNameMaybe :: String -> IO (Maybe UserEntry);
    getUserEntryForNameMaybe s = checkUserEntry (getUserEntryForName s);

    getUserEntryForIDMaybe :: UserID -> IO (Maybe UserEntry);
    getUserEntryForIDMaybe uid = checkUserEntry (getUserEntryForID uid);

    processFunctions :: (?context :: String) => String -> Maybe Value;
    processFunctions "context-get" = Just (toValue getContext);
    processFunctions "context-with" = Just (toValue withContext);
    processFunctions "exec-start" = Just (toValue startProcess);
    processFunctions "exec-run" = Just (toValue runProcess);
    processFunctions "userentry-id-get" = Just (toValue getUserEntryForIDMaybe);
    processFunctions "userentry-name-get" = Just (toValue getUserEntryForNameMaybe);
    processFunctions _ = Nothing;
}
