{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Argo.StdLib.Process(startProcess,runProcess) where
{
    import Import;
    import System.Exit;
    import System.Posix.Types;
    import System.Posix.Process;
    import System.Posix.Signals;
    import Data.Argo.SubValue;
    import Data.Argo.Value;


    instance ToValue Value ExitCode where
    {
        toValue ExitSuccess = toValue ();
        toValue (ExitFailure code) = toValue code;
    };

    processStatus :: ProcessStatus -> Maybe String -> Value;
    processStatus (Exited _) Nothing = toValue "exited";
    processStatus (Exited code) (Just "error") = toValue code;
    processStatus (Terminated _) Nothing = toValue "terminated";
    processStatus (Terminated signal) (Just "signal") = toValue signal;
    processStatus (Stopped _) Nothing = toValue "stopped";
    processStatus (Stopped signal) (Just "signal") = toValue signal;
    processStatus _ (Just _) = toValue ();

    instance ToValue Value ProcessStatus where
    {
        toValue ps = toValue (processStatus ps);
    };

    waitProcess :: ProcessID -> IO ProcessStatus;
    waitProcess pid = do
    {
        mps <- getProcessStatus True False pid;
        case mps of
        {
            Just ps -> return ps;
            Nothing -> fail "process still running after wait";
        };
    };

    processID :: ProcessID -> String -> Value;
    processID (CPid pid) "id" = toValue pid;
    processID pid "check" = toValue (getProcessStatus False True pid);
    processID pid "wait" = toValue (waitProcess pid);
    processID pid "signal" = toValue (\signal -> signalProcess signal pid);
    processID pid "terminate" = toValue (signalProcess softwareTermination pid);
    processID pid "kill" = toValue (signalProcess killProcess pid);
    processID _ _ = toValue ();

    instance ToValue Value ProcessID where
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
            executeFile cmdpath False args env;
        });
    };
    
    runProcess :: (?context :: String) => (Maybe String -> Value) -> IO ProcessStatus;
    runProcess fargs = do
    {
        pid <- startProcess fargs;
        waitProcess pid;
    };
}
