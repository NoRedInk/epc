{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.Foldable (traverse_)
import Data.Time.Clock (getCurrentTime)
import ElmFormat.Render.Text (render)
import ElmVersion (ElmVersion(Elm_0_16))
import Options.Applicative (execParser)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), joinPath)
import Trace.Epc.Mix (mkMix)
import Trace.Epc.Options (Options(Options), epcdir, file, opts)
import Trace.Epc.Parse (parse)
import Trace.Epc.Tix (mkTix)
import Trace.Hpc.Mix (mixCreate)
import Trace.Hpc.Tix (writeTix)
import Data.List (intercalate)

import qualified Data.Text.IO as T

main :: IO ()
main = do
    Options {epcdir, file} <- execParser opts
    source <- readFile file
    time <- getCurrentTime
    let (entries, names, instrumented) = parse source
    createDirectoryIfMissing True (epcdir </> "Instrumented" </> joinPath (init names))
    let name = intercalate "." names
    mixCreate epcdir name $ mkMix file time entries
    traverse_ (T.writeFile (epcdir </> "Instrumented" </> joinPath names <.> "elm" ) . render Elm_0_16) instrumented
    writeTix (epcdir </> name <.> "tix") $ mkTix name entries
