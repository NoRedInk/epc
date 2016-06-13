module Trace.Epc.Parse where

import AST.Module (Module)
import Control.Monad.Reader (runReader)
import Parse.Parse (parseSource)
import Reporting.Result (Result(Result), RawResult(Ok))
import Trace.Epc.Instrument (instrumentModule)
import Trace.Epc.Mix (mixEntries, moduleName)
import Trace.Hpc.Mix (MixEntry)
import Data.List (intercalate)

parse :: String -> ([MixEntry], [String], Maybe Module)
parse str =
    case parseSource str of
        Result _ (Ok m) ->
            let names = moduleName m
                instrumented = instrumentModule m
                entries = mixEntries m
            in (entries, names, Just $ runReader instrumented (intercalate "." names, entries))
        _ -> ([], [], Nothing)
