--
-- Copyright (c) 2021 Alexandre Joannou
-- All rights reserved.
--
-- This material is based upon work supported by the DoD Information Analysis
-- Center Program Management Office (DoD IAC PMO), sponsored by the Defense
-- Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
-- opinions, findings and conclusions or recommendations expressed in this
-- material are those of the author(s) and do not necessarily reflect the views
-- of the Air Force Installation Contracting Agency (AFICA).
--
-- @BERI_LICENSE_HEADER_START@
--
-- Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
-- license agreements.  See the NOTICE file distributed with this work for
-- additional information regarding copyright ownership.  BERI licenses this
-- file to you under the BERI Hardware-Software License, Version 1.0 (the
-- "License"); you may not use this file except in compliance with the
-- License.  You may obtain a copy of the License at:
--
--   http://www.beri-open-systems.org/legal/license-1-0.txt
--
-- Unless required by applicable law or agreed to in writing, Work distributed
-- under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
-- CONDITIONS OF ANY KIND, either express or implied.  See the License for the
-- specific language governing permissions and limitations under the License.
--
-- @BERI_LICENSE_HEADER_END@
--

{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Text.Read
import System.IO
import System.Exit
import Control.Monad
import System.Environment
import System.Console.GetOpt
import qualified Data.Map as M
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import VIPBundle.Types
import VIPBundle.Parse
import VIPBundle.Pretty_QUARTUS_IP_TCL
import VIPBundle.InterfaceInference

-- command line arguments
--------------------------------------------------------------------------------

data OutputFormat = QUARTUS_IP_TCL | TXT | JSON

instance Read OutputFormat where
  readPrec = do Ident "quartus_ip_tcl" <- lexP
                return QUARTUS_IP_TCL
             +++
             do Ident "txt" <- lexP
                return TXT
             +++
             do Ident "json" <- lexP
                return JSON

data Options = Options { optOutputFile   :: Maybe FilePath
                       , optOutputFormat :: OutputFormat
                       , optHelpAndQuit  :: Bool }

defaultOptions :: Options
defaultOptions = Options { optOutputFile   = Nothing
                         , optOutputFormat = TXT
                         , optHelpAndQuit  = False }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['o'] ["output-file"]
           (ReqArg (\arg opts -> opts { optOutputFile = Just arg })
                   "FILEPATH")
           "specify a FILEPATH for the output file"
  , Option ['f'] ["output-format"]
           (ReqArg (\arg opts -> opts { optOutputFormat = read arg })
                   "OUTPUTFORMAT")
           "specify desired OUTPUTFORMAT, one of quartus_ip_tcl, txt (default)"
  , Option ['h'] ["help"] (NoArg \opts -> opts { optHelpAndQuit = True })
           "display help"
  ]

helpMsg :: String
helpMsg = usageInfo header options
  where header = "Usage: vipbundle [OPTION...] files..."

commandOpts :: [String] -> IO (Options, [String])
commandOpts argv =
  case getOpt Permute options argv of
    (optUpdtFns, posArgs, []) ->
      return (foldl (flip id) defaultOptions optUpdtFns, posArgs)
    (_, _, errs) ->
      ioError (userError (concat errs ++ helpMsg))

main :: IO ()
main = do
  -- parse command line arguments
  rawArgs <- getArgs
  (opts, posArgs) <- commandOpts rawArgs
  -- handle help case
  when (optHelpAndQuit opts) do putStrLn helpMsg
                                exitSuccess
  --
  (inptHandle, mInptFileName) <-
    case posArgs of f:_ -> openFile f ReadMode >>= \x -> return (x, Just f)
                    [] -> return (stdin, Nothing)
  outHandle <- case optOutputFile opts of Just f -> openFile f WriteMode
                                          Nothing -> return stdout
  let pretty = case optOutputFormat opts of
                 QUARTUS_IP_TCL -> pretty_QUARTUS_IP_TCL
                 JSON -> BS.unpack . Aeson.encode
                 _ -> show
  --
  allVerilogModules <- parseVerilog mInptFileName <$> hGetContents inptHandle
  let allModIfcs = inferInterfaces mInptFileName <$> allVerilogModules
  forM_ allModIfcs \x -> do
    hPutStrLn outHandle $ pretty x
  --
  hClose inptHandle
  hClose outHandle
