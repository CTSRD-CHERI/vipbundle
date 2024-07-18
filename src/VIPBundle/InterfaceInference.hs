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

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module VIPBundle.InterfaceInference (
  inferInterfaces
) where

import Data.Maybe
import Data.STRef
import Data.Foldable
import Control.Monad
import Data.List.Split
import Text.Regex.TDFA
import Control.Monad.ST
import Data.Map qualified as M

import VIPBundle.Types

-- Regex helpers
----------------
type RegexRetType = (String, String, String, [String])
pattern RegexMatches subs <- (_, _, _, subs)

detectIgnorePort :: RichPort -> Maybe RichPort
detectIgnorePort p =
  case p.identifier =~ "\\<(IGNORE|ignore)(.*)?" :: RegexRetType of
    RegexMatches ["IGNORE",_] -> Just p{ typeIfc = Ignore }
    RegexMatches ["ignore",_] -> Just p{ typeIfc = Ignore }
    _ -> Nothing

detectClockPort :: RichPort -> Maybe RichPort
detectClockPort p =
  case p.identifier =~ "\\<(cs(i|o)|clk|CLK)(_(.*))?" :: RegexRetType of
    RegexMatches ["cs","i",_,_] -> Just $ rp Sink
    RegexMatches ["cs","o",_,_] -> Just $ rp Source
    RegexMatches [   _,  _,_,_] -> Just $ rp Sink
    _ -> Nothing
  where rp d = p { typeIfc = Clock, clockIfc = Nothing, resetIfc = Nothing }

detectResetPort :: RichPort -> Maybe RichPort
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
    regex = "\\<(rs(i|o)|rst|RST)(.*_(n|N))?(_(.*))?"
    rp d n = p { typeIfc = Reset n, resetIfc = Nothing }

detectAXI4Port :: RichPort -> Maybe RichPort
detectAXI4Port p =
  case p.identifier =~ regex :: RegexRetType of
    RegexMatches [pfx, mOrS, _, ifcnm, signm] ->
      let ifctype = case pfx of "str" -> AXI4Stream
                                "l" -> AXI4Lite
                                _ -> AXI4
          dir = if mOrS == "m" then Master else Slave
          ifcname = case ifcnm of "" | dir == Master -> "axi4_m"
                                  "" | dir == Slave -> "axi4_s"
                                  nm -> nm
      in Just p { typeIfc = ifctype
                , identIfc = ifcname
                , identSig = signm
                }
    _ -> Nothing
  where
    regex = "\\<ax(l|str)?([ms])_((.+)_)*(.+)"

detectAvalonPort :: RichPort -> Maybe RichPort
detectAvalonPort p =
  case p.identifier =~ regex :: RegexRetType of
    RegexMatches [mOrS, _, ifcnm, signm] ->
      let dir = if mOrS == "m" then Master else Slave
          ifcname = case ifcnm of "" | dir == Master -> "av_host"
                                  "" | dir == Slave -> "av_agent"
                                  nm -> nm
      in Just p { typeIfc = Avalon
                , identIfc = ifcname
                , identSig = signm
                }
    _ -> Nothing
  where
    regex = "\\<av([ms])_((.+)_)*(.+)"

detectIrqPort :: RichPort -> Maybe RichPort
detectIrqPort p =
  case p.identifier =~ "\\<in([sr])(_(.*))?" :: RegexRetType of
    RegexMatches matches -> go matches
    _ -> Nothing
  where go ["s", _, _] = Just $ rp Sender
        go ["r", _, _] = Just $ rp Receiver
        go _ = Nothing
        rp d = p { typeIfc = Irq }

detectConduitIfcPort :: RichPort -> Maybe RichPort
detectConduitIfcPort p =
  case p.identifier =~ regex :: RegexRetType of
    RegexMatches [sOrE, _, ifcnm, signm] ->
      let dir = if sOrE == "s" then Start else End
          ifcname = case ifcnm of "" | dir == Start -> "conduitIfcStart"
                                  "" | dir == End -> "conduitIfcEnd"
                                  nm -> nm
      in Just p { identIfc = ifcname
                , identSig = signm
                }
    _ -> Nothing
  where
    regex = "\\<ci([se])_((.+)_)*(.+)"

detectConduitPort :: RichPort -> Maybe RichPort
detectConduitPort = Just

extractField :: String -> String -> (Maybe String, String)
extractField delimiter string =
  case splitOn delimiter string of
    [pfx, fld, sfx] -> (Just fld, pfx ++ sfx)
    [original] -> (Nothing, original)

data IdentifierSplit = IdentifierSplit { clock :: Maybe String
                                       , reset :: Maybe String
                                       , rest :: String
                                       }

splitIdentifier :: String -> IdentifierSplit
splitIdentifier ident =
  let (mClk,  ident') = extractField "_C_" ident
      (mRst, ident'') = extractField "_R_" ident'
  in IdentifierSplit { clock = mClk
                     , reset = mRst
                     , rest = ident''
                     }

detectPortIfc :: VerilogPort -> RichPort
detectPortIfc vp = rp'
  where idSplit = splitIdentifier vp.identifier
        rp = RichPort { rawIdentifier = vp.identifier
                      , identifier = idSplit.rest
                      , direction = vp.direction
                      , width = vp.width
                      , typeIfc = Conduit
                      , identIfc = idSplit.rest
                      , identSig = "conduit_" ++ show vp.width
                      , clockIfc = idSplit.clock
                      , resetIfc = idSplit.reset
                      }
        rp' = fromMaybe (error "port detection error") $
                asum [ detectIgnorePort rp
                     , detectClockPort rp
                     , detectResetPort rp
                     , detectAXI4Port rp
                     , detectAvalonPort rp
                     , detectIrqPort rp
                     , detectConduitIfcPort rp
                     , detectConduitPort rp ]

detectIfcs :: [RichPort] -> M.Map String Ifc
detectIfcs = go M.empty
  where go mp [] = mp
        go mp (p:ps) = go (M.alter (updtIfc p) p.identIfc mp) ps
        updtIfc p Nothing = Just $ Ifc [p]
        updtIfc p (Just (Ifc ps)) = Just $ Ifc (p:ps)

inferInterfaces :: Maybe FilePath -> VerilogModule -> RichModule
inferInterfaces mfp m = RichModule m.name modIfcs mfp
  where modIfcs = detectIfcs . (detectPortIfc <$>) $ m.ports
