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

module VIPBundle.Types (
  PortDir (..)
, IfcType (..)
, VerilogPort (..)
, VerilogModule (..)
, VerilogPortWithIfc (..)
, Ifc (..)
, VerilogModuleWithIfc (..)
) where

import Prelude hiding ((<>))
import Data.Map hiding (empty)
import Text.PrettyPrint

data PortDir = In | Out deriving Eq
instance Show PortDir where
  show In  = "Input"
  show Out = "Output"
data IfcType = Clock | Reset | AXI4 | AXI4Lite | Conduit deriving Eq
instance Show IfcType where
  show Clock = "clock"
  show Reset = "reset"
  show AXI4 = "axi4"
  show AXI4Lite = "axi4lite"
  show Conduit = "conduit"

data VerilogPort = VerilogPort {
  portName      :: String
, portDirection :: PortDir
, portWidth     :: Integer }
docVerilogPort :: VerilogPort -> Doc
docVerilogPort VerilogPort{..} =
  text portName <+>
  (braces . sep . punctuate comma)
    [ text "width:" <+> integer portWidth
    , text "dir:"   <+> text (show portDirection) ]
instance Show VerilogPort where show = render . docVerilogPort

data VerilogModule = VerilogModule {
  modName  :: String
, modPorts :: [VerilogPort] }
docVerilogModule :: VerilogModule -> Doc
docVerilogModule VerilogModule{..} =
  hang (text modName <> colon) 2 (sep $ fmap docVerilogPort modPorts)
instance Show VerilogModule where show = render . docVerilogModule

data VerilogPortWithIfc =
    ClockPort VerilogPort
  | ResetPort Bool VerilogPort
  | AXI4MPort String String VerilogPort -- ifc name, axi4 standard sig name, full sig
  | AXI4SPort String String VerilogPort -- ifc name, axi4 standard sig name, full sig
  | AXI4LiteMPort String String VerilogPort -- ifc name, axi4 standard sig name, full sig
  | AXI4LiteSPort String String VerilogPort -- ifc name, axi4 standard sig name, full sig
  | ConduitPort VerilogPort
docVerilogPortWithIfc :: VerilogPortWithIfc -> Doc
docVerilogPortWithIfc (ClockPort vp) =
  hsep [ text "Clock", text "--", docVerilogPort vp ]
docVerilogPortWithIfc (ResetPort activLo vp) =
  hsep [ text "Reset"
       , if activLo then text "[Active Low]" else empty
       , text "--", docVerilogPort vp ]
docVerilogPortWithIfc (AXI4MPort _ sNm vp) =
  hsep [ text "AXI4 Master", text "-"
       , text sNm, text "--"
       , docVerilogPort vp ]
docVerilogPortWithIfc (AXI4SPort _ sNm vp) =
  hsep [ text "AXI4 Slave", text "-"
       , text sNm, text "--"
       , docVerilogPort vp ]
docVerilogPortWithIfc (AXI4LiteMPort _ sNm vp) =
  hsep [ text "AXI4Lite Master", text "-"
       , text sNm, text "--"
       , docVerilogPort vp ]
docVerilogPortWithIfc (AXI4LiteSPort _ sNm vp) =
  hsep [ text "AXI4Lite Slave", text "-"
       , text sNm, text "--"
       , docVerilogPort vp ]
docVerilogPortWithIfc (ConduitPort vp) =
  hsep [ text "<no interface>", text "--", docVerilogPort vp ]
instance Show VerilogPortWithIfc where show = render . docVerilogPortWithIfc

data Ifc = Ifc {
  ifcClock :: Maybe VerilogPortWithIfc
, ifcReset :: Maybe VerilogPortWithIfc
, ifcPorts :: [VerilogPortWithIfc]
, ifcType  :: IfcType }
docIfc :: Ifc -> Doc
docIfc Ifc{..} =
  hang (text (show ifcType) <+> text "(interface)") 2
       (vcat [ case ifcClock of
                 Just p -> text "associated clock:" <+> docVerilogPortWithIfc p
                 Nothing -> text "no associated clock" , case ifcReset of
                 Just p -> text "associated reset:" <+> docVerilogPortWithIfc p
                 Nothing -> text "no associated reset"
             , sep $ (text "ports:") : fmap docVerilogPortWithIfc ifcPorts ])
instance Show Ifc where show = render . docIfc

data VerilogModuleWithIfc = VerilogModuleWithIfc {
  richModName    :: String
, richModIfcs    :: Map String Ifc
, richModTopFile :: Maybe FilePath }
docVerilogModuleWithIfc :: VerilogModuleWithIfc -> Doc
docVerilogModuleWithIfc VerilogModuleWithIfc{..} =
  hang (hsep [text "--", text richModName, text "(module)", text "--"]) 2
       (vcat $ topfileline : fmap prettyIfc (toList richModIfcs))
  where prettyIfc (nm, ifc) =
          text "*" <+> text nm <> colon <+> docIfc ifc
        topfileline = case richModTopFile of
                        Just f -> text "top file: " <> text f
                        _ -> empty
instance Show VerilogModuleWithIfc where show = render . docVerilogModuleWithIfc
