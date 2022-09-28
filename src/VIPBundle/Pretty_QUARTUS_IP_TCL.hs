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

{-# LANGUAGE RecordWildCards #-}

module VIPBundle.Pretty_QUARTUS_IP_TCL (
  pretty_QUARTUS_IP_TCL
) where

import System.FilePath.Posix
import Data.Map hiding (empty)
import Text.PrettyPrint

import VIPBundle.Types

comment doc = char '#' <+> doc

prettyVerilogModuleWithIfc :: VerilogModuleWithIfc -> Doc
prettyVerilogModuleWithIfc VerilogModuleWithIfc{..} =
  vcat $ pkgReq : modDefs : fileSetDefs : ifcsDefs
  where
    -- TCL package require
    pkgReq = text "package require qsys"
    -- Module level definitions
    modDefs = vcat [ comment (text "module:" <+> text richModName)
                   , mProp "NAME" richModName
                   , mProp "DISPLAY_NAME" richModName ]
    -- File Sets definitions
    fileSetNm = richModName ++ "_fileset"
    fileSetDefs = case richModTopFile of
      Just f -> vcat [ comment $ text "file set"
                     , hsep [ text "add_fileset"
                            , text fileSetNm
                            , text "QUARTUS_SYNTH" ]
                     , hsep [ text "set_fileset_property"
                            , text fileSetNm
                            , text "TOP_LEVEL"
                            , text richModName ]
                     , hsep [ text "add_fileset_file"
                            , text $ takeFileName f
                            , text "VERILOG"
                            , text "PATH"
                            , text f
                            , text "TOP_LEVEL_FILE" ] ]
      _ -> empty
    -- Sub-Interface level definitions
    ifcsDefs = ifcDefs <$> toList richModIfcs
    ifcDefs (iNm, ifc@Ifc{..}) =
      vcat $ [ comment (text "interface:" <+> text iNm)
             , iAdd iNm ifc
             , iProp iNm "ENABLED" "true"
             , case ifcClock of Just clk -> iAssocClk iNm clk
                                _ -> empty
             , case ifcReset of Just rst -> iAssocRst iNm rst
                                _ -> empty
             , case ifcType of
                 Reset -> iRstPolarity iNm $ head ifcPorts
                 _ -> empty ] ++ fmap (iIfcPort iNm) ifcPorts
    -- query helpers
    isAXI4MIfc Ifc{..} =
      ifcType == AXI4 && case head ifcPorts of AXI4MPort _ _ _ -> True
                                               _ -> False
    isAXI4SIfc Ifc{..} =
      ifcType == AXI4 && case head ifcPorts of AXI4SPort _ _ _ -> True
                                               _ -> False
    isAXI4LiteMIfc Ifc{..} =
      ifcType == AXI4Lite && case head ifcPorts of AXI4LiteMPort _ _ _ -> True
                                                   _ -> False
    isAXI4LiteSIfc Ifc{..} =
      ifcType == AXI4Lite && case head ifcPorts of AXI4LiteSPort _ _ _ -> True
                                                   _ -> False
    isIrqSIfc Ifc{..} =
      ifcType == Irq && case head ifcPorts of IrqSenderPort _ -> True
                                              _ -> False
    isIrqRIfc Ifc{..} =
      ifcType == Irq && case head ifcPorts of IrqReceiverPort _ -> True
                                              _ -> False
    isClockSink Ifc{..} =
      ifcType == Clock && case head ifcPorts of ClockSinkPort _ -> True
                                                _ -> False
    isClockSource Ifc{..} =
      ifcType == Clock && case head ifcPorts of ClockSourcePort _ -> True
                                                _ -> False
    isResetSink Ifc{..} =
      ifcType == Reset && case head ifcPorts of ResetSinkPort _ _ -> True
                                                _ -> False
    isResetSource Ifc{..} =
      ifcType == Reset && case head ifcPorts of ResetSourcePort _ _ -> True
                                                _ -> False
    -- Quartus platform designer command helpers
    iAssocClk iNm clk@(ClockSinkPort VerilogPort{..})
      | portDirection == In && portWidth == 1 =
        iProp iNm "associatedClock" portName
      | otherwise = error $ "broken clock: " ++ show clk
    iAssocRst iNm rst@(ResetSinkPort _ VerilogPort{..})
      | portDirection == In && portWidth == 1 =
        iProp iNm "associatedReset" portName
      | otherwise = error $ "broken reset: " ++ show rst
    iRstPolarity iNm (ResetSinkPort deAssrt _) =
      iProp iNm "synchronousEdges" $ if deAssrt then "DEASSERT" else "ASSERT"
    iRstPolarity iNm (ResetSourcePort deAssrt _) =
      iProp iNm "synchronousEdges" $ if deAssrt then "DEASSERT" else "ASSERT"
    iIfcPort iNm (ClockSourcePort VerilogPort{..}) =
      iPort iNm portName "clk" (show portDirection) portWidth
    iIfcPort iNm (ClockSinkPort VerilogPort{..}) =
      iPort iNm portName "clk" (show portDirection) portWidth
    iIfcPort iNm (ResetSinkPort pol VerilogPort{..}) =
      iPort iNm portName ("reset" ++ if pol then "_n" else "")
                         (show portDirection) portWidth
    iIfcPort iNm (ResetSourcePort pol VerilogPort{..}) =
      iPort iNm portName ("reset" ++ if pol then "_n" else "")
                         (show portDirection) portWidth
    iIfcPort iNm (AXI4MPort _ sNm VerilogPort{..}) =
      iPort iNm portName sNm (show portDirection) portWidth
    iIfcPort iNm (AXI4SPort _ sNm VerilogPort{..}) =
      iPort iNm portName sNm (show portDirection) portWidth
    iIfcPort iNm (AXI4LiteMPort _ sNm VerilogPort{..}) =
      iPort iNm portName sNm (show portDirection) portWidth
    iIfcPort iNm (AXI4LiteSPort _ sNm VerilogPort{..}) =
      iPort iNm portName sNm (show portDirection) portWidth
    iIfcPort iNm (IrqSenderPort VerilogPort{..}) =
      iPort iNm portName "irq" (show portDirection) portWidth
    iIfcPort iNm (IrqReceiverPort VerilogPort{..}) =
      iPort iNm portName "irq" (show portDirection) portWidth
    iIfcPort iNm (ConduitPort VerilogPort{..}) =
      iPort iNm portName portName (show portDirection) portWidth
    -- generic Quartus platform designer command helpers
    mProp nm val = hsep [ text "set_module_property", text nm, text val ]
    iAdd nm ifc@Ifc{..} =
      hsep [ text "add_interface"
           , text nm
           , text (show ifcType)
           , case ifcType of
               AXI4 | isAXI4MIfc ifc -> text "master"
               AXI4 | isAXI4SIfc ifc -> text "slave"
               AXI4Lite | isAXI4LiteMIfc ifc -> text "master"
               AXI4Lite | isAXI4LiteSIfc ifc -> text "slave"
               Irq | isIrqRIfc ifc -> text "start"
               Clock | isClockSink ifc -> text "end"
               Clock | isClockSource ifc -> text "start"
               Reset | isResetSink ifc -> text "end"
               Reset | isResetSource ifc -> text "start"
               _ -> text "end" ]
    iProp iNm pNm val =
      hsep [ text "set_interface_property", text iNm, text pNm, text val ]
    iPort iNm pNm sNm dir w = hsep [ text "add_interface_port"
                                   , text iNm, text pNm, text sNm
                                   , text dir, integer w ]

pretty_QUARTUS_IP_TCL :: VerilogModuleWithIfc -> String
pretty_QUARTUS_IP_TCL = render . prettyVerilogModuleWithIfc
