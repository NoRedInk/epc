module Trace.Epc.Parse where

import AST.Module (Module)
import Control.Monad.Reader (runReader)
import Parse.Parse (parseSource)
import Reporting.Result (Result(Result), RawResult(Ok))
import Trace.Epc.Instrument (instrumentModule)
import Trace.Epc.Mix (mixEntries, moduleName)
import Trace.Hpc.Mix (MixEntry)

parse :: String -> ([MixEntry], String, Maybe Module)
parse str =
    case parseSource str of
        Result _ (Ok m) ->
            let name = moduleName m
                instrumented = instrumentModule m
                entries = mixEntries m
            in (entries, name, Just $ runReader instrumented (name, entries))
        _ -> ([], [], Nothing)
