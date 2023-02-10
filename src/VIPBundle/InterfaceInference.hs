--
-- Copyright (c) 2021-2023 Alexandre Joannou
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

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE OverloadedRecordDot #-}

module VIPBundle.InterfaceInference (
  inferInterfaces
) where

import Data.Maybe
import Data.STRef
import Data.Foldable
import Control.Monad
import Text.Regex.TDFA
import Control.Monad.ST
import qualified Data.Map as M

import VIPBundle.Types

-- Regex helpers
----------------
type RegexRetType = (String, String, String, [String])
pattern RegexMatches subs <- (_, _, _, subs)

-- Generare QSYS tcl file
-------------------------

detectClockPort :: VerilogPort -> Maybe RichPort
detectClockPort p =
  case p.identifier =~ "\\<(cs(i|o)|clk|CLK)(_(.*))?" :: RegexRetType of
    RegexMatches ["cs","i",_,_] -> Just $ rp Sink
    RegexMatches ["cs","o",_,_] -> Just $ rp Source
    RegexMatches [   _,  _,_,_] -> Just $ rp Sink
    _ -> Nothing
  where rp d = RichPort { identifier = p.identifier
                        , direction = d
                        , width = p.width
                        , typeIfc = Clock
                        , identIfc = p.identifier
                        , identSig = p.identifier
                        , clockIfc = Nothing
                        , resetIfc = Nothing
                        }

detectResetPort :: VerilogPort -> Maybe RichPort
detectResetPort p =
  case p.identifier =~ regex :: RegexRetType of
    RegexMatches ["rs","i",_,"",_,_] -> Just $ rp   Sink False
    RegexMatches ["rs","o",_,"",_,_] -> Just $ rp Source False
    RegexMatches ["rs","i",_, _,_,_] -> Just $ rp   Sink  True
    RegexMatches ["rs","o",_, _,_,_] -> Just $ rp Source  True
    RegexMatches [  _,   _,_,"",_,_] -> Just $ rp   Sink False
    RegexMatches [  _,   _,_, _,_,_] -> Just $ rp   Sink  True
    _ -> Nothing
  where
    regex = "\\<(rs(i|o)|rst|RST)(_(n|N))?(_(.*))?"
    rp d n = RichPort { identifier = p.identifier
                      , direction = d
                      , width = p.width
                      , typeIfc = Reset n
                      , identIfc = p.identifier
                      , identSig = p.identifier
                      , clockIfc = Nothing
                      , resetIfc = Nothing
                      }

detectAXI4Port :: VerilogPort -> Maybe RichPort
detectAXI4Port p =
  case p.identifier =~ regex :: RegexRetType of
    RegexMatches [pfx, mOrs, _, ifcnm, signm, _, clk, _, rst] ->
      let ifctype = case pfx of "str" -> AXI4Stream
                                "l" -> AXI4Lite
                                _ -> AXI4
          dir = if mOrs == "m" then Master else Slave
          ifcname = case ifcnm of "" | dir == Master -> "axi4_m"
                                  "" | dir == Slave -> "axi4_s"
                                  nm -> nm
          mclk = if clk == "" then Nothing else Just clk
          mrst = if rst == "" then Nothing else Just clk
      in Just RichPort { identifier = p.identifier
                       , direction = dir
                       , width = p.width
                       , typeIfc = ifctype
                       , identIfc = ifcname
                       , identSig = signm
                       , clockIfc = mclk
                       , resetIfc = mrst
                       }
    _ -> Nothing
  where
    regex = "\\<ax(l|str)?([ms])_((.+)_)*(.+)(_C(.+))?(_R(.+))?"

detectIrqPort :: VerilogPort -> Maybe RichPort
detectIrqPort p =
  case p.identifier =~ "\\<in([sr])(_(.*))?" :: RegexRetType of
    RegexMatches matches -> go matches
    _ -> Nothing
  where go ["s", _, _] = Just $ rp Sender
        go ["r", _, _] = Just $ rp Receiver
        go _ = Nothing
        rp d = RichPort { identifier = p.identifier
                        , direction = d
                        , width = p.width
                        , typeIfc = Irq
                        , identIfc = p.identifier
                        , identSig = p.identifier
                        , clockIfc = Nothing
                        , resetIfc = Nothing
                        }


detectConduitPort :: VerilogPort -> Maybe RichPort
detectConduitPort p =
  case p.identifier =~ ".*(_C(.+))?(_R(.+))?" :: RegexRetType of
    RegexMatches [_, clk, _, rst] ->
      let mclk = if clk == "" then Nothing else Just clk
          mrst = if rst == "" then Nothing else Just rst
      in Just $ rp mclk mrst
    _ -> Just $ rp Nothing Nothing
  where rp mclk mrst = RichPort { identifier = p.identifier
                                , direction = p.direction
                                , width = p.width
                                , typeIfc = Conduit
                                , identIfc = p.identifier
                                , identSig = p.identifier
                                , clockIfc = mclk
                                , resetIfc = mrst
                                }

detectPortIfcs :: [VerilogPort] -> [RichPort]
detectPortIfcs = fmap (fromMaybe (error "port detection error") . detectIfc)
  where detectIfc p = asum [ detectClockPort p
                           , detectResetPort p
                           , detectAXI4Port p
                           , detectIrqPort p
                           , detectConduitPort p ]

detectIfcs :: [RichPort] -> M.Map String Ifc
detectIfcs = go M.empty
  where go mp [] = mp
        go mp (p:ps) = go (M.alter (updtIfc p) p.identIfc mp) ps
        updtIfc p Nothing = Just $ Ifc [p]
        updtIfc p (Just (Ifc ps)) = Just $ Ifc (p:ps)

inferInterfaces :: Maybe FilePath -> VerilogModule -> RichModule
inferInterfaces mfp m = RichModule m.name modIfcs mfp
  where modIfcs = detectIfcs . detectPortIfcs $ m.ports
