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
{-# LANGUAGE PatternSynonyms #-}

module VIPBundle.InterfaceInference (
  inferInterfaces
) where

import Data.Maybe
import Data.STRef
import Data.Foldable
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

type DetectPort = VerilogPort -> Maybe VerilogPortWithIfc

detectClockPort :: DetectPort
detectClockPort p@VerilogPort{..} =
  if portName =~ "\\<(clk|CLK)(_(.*))?" then Just $ ClockPort p else Nothing

detectResetPort :: DetectPort
detectResetPort p@VerilogPort{..} =
  case portName =~ "\\<(rst|RST)(_(n|N))?(_(.*))?" :: RegexRetType of
    RegexMatches [_,_,"",_,_] -> Just $ ResetPort False p
    RegexMatches [_,_, _,_,_] -> Just $ ResetPort  True p
    _ -> Nothing

detectAXI4Port :: DetectPort
detectAXI4Port p@VerilogPort{..} =
  case portName =~ "\\<ax(l?)([ms])_((.+)_)*(.+)" :: RegexRetType of
    RegexMatches matches -> go matches
    _ -> Nothing
  where go ["", "m", _,    "", signm] = Just $ AXI4MPort "axi4_m" signm p
        go ["", "s", _,    "", signm] = Just $ AXI4SPort "axi4_s" signm p
        go ["", "m", _, ifcnm, signm] = Just $ AXI4MPort ifcnm signm p
        go ["", "s", _, ifcnm, signm] = Just $ AXI4SPort ifcnm signm p
        go ["l", "m", _,    "", signm] = Just $ AXI4LiteMPort "axi4_m" signm p
        go ["l", "s", _,    "", signm] = Just $ AXI4LiteSPort "axi4_s" signm p
        go ["l", "m", _, ifcnm, signm] = Just $ AXI4LiteMPort ifcnm signm p
        go ["l", "s", _, ifcnm, signm] = Just $ AXI4LiteSPort ifcnm signm p
        go _ = Nothing

detectConduitPort :: DetectPort
detectConduitPort p = Just $ ConduitPort p

detectPortIfcs :: [VerilogPort] -> [VerilogPortWithIfc]
detectPortIfcs = fmap (fromMaybe (error "port detection error") . detectIfc)
  where detectIfc p = asum [ detectClockPort p
                           , detectResetPort p
                           , detectAXI4Port p
                           , detectConduitPort p ]

detectIfcs :: [VerilogPortWithIfc] -> M.Map String Ifc
detectIfcs ports = runST do
  currentClock <- newSTRef Nothing
  currentReset <- newSTRef Nothing
  go currentClock currentReset ports M.empty
  where go _ _ [] mp = return mp
        go clkRef rstRef (p:ps) mp = do
          clk <- readSTRef clkRef
          rst <- readSTRef rstRef
          (nm, ifc) <- case p of
            ClockPort vp -> do writeSTRef clkRef $ Just p
                               return (portName vp, newClkIfc)
            ResetPort _ vp -> do writeSTRef rstRef $ Just p
                                 return (portName vp, newRstIfc clk)
            AXI4MPort iNm _ _ ->
              return (iNm, fromMaybe (newAXI4Ifc clk rst) (M.lookup iNm mp))
            AXI4SPort iNm _ _ ->
              return (iNm, fromMaybe (newAXI4Ifc clk rst) (M.lookup iNm mp))
            AXI4LiteMPort iNm _ _ ->
              return (iNm, fromMaybe (newAXI4LiteIfc clk rst) (M.lookup iNm mp))
            AXI4LiteSPort iNm _ _ ->
              return (iNm, fromMaybe (newAXI4LiteIfc clk rst) (M.lookup iNm mp))
            ConduitPort vp -> return (portName vp, newConduitIfc clk rst)
          go clkRef rstRef ps (M.insert nm ifc{ifcPorts = p : ifcPorts ifc} mp)
        newClkIfc              = Ifc Nothing Nothing [] Clock
        newRstIfc      clk     = Ifc     clk Nothing [] Reset
        newAXI4Ifc     clk rst = Ifc     clk     rst [] AXI4
        newAXI4LiteIfc clk rst = Ifc     clk     rst [] AXI4Lite
        newConduitIfc  clk rst = Ifc     clk     rst [] Conduit

inferInterfaces :: VerilogModule -> VerilogModuleWithIfc
inferInterfaces VerilogModule{..} = VerilogModuleWithIfc modName modIfcs
  where modIfcs = detectIfcs . detectPortIfcs $ modPorts
