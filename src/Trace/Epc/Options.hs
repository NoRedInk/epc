module Trace.Epc.Options where

import Data.Monoid ((<>))
import Options.Applicative
    ( Parser, ParserInfo, InfoMod
    , fullDesc, header, help, helper, info, long, metavar
    , progDesc, strArgument, strOption, value
    )

data Options = Options { epcdir :: String, file :: String }

options :: Parser Options
options =
    Options <$> strOption (long "epcdir" <> metavar "DIR" <> value ".epc" <> help "Directory containing instrumented files")
            <*> strArgument (metavar "FILE")

opts :: ParserInfo Options
opts =
    info (helper <*> options) description

description :: InfoMod a
description =
    fullDesc <> progDesc "Instrument an elm file for code coverage"
             <> header "epc - elm program coverage"
