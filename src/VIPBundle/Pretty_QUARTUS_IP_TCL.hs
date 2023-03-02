{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

{-# LANGUAGE RecordWildCards #-}

module VIPBundle.Pretty_QUARTUS_IP_TCL (
  pretty_QUARTUS_IP_TCL
) where

import System.FilePath.Posix
import Data.Map qualified as M
import Text.PrettyPrint

import VIPBundle.Types

comment doc = char '#' <+> doc

scanlWithClkRst :: ( (Maybe IfcIdentifier, Maybe IfcIdentifier)
                     -> (IfcIdentifier, Ifc)
                     -> a ) -> [(IfcIdentifier, Ifc)] -> [a]
scanlWithClkRst f ifcs =
  (snd <$>) . tail $ scanl f' ((Nothing, Nothing), undefined) ifcs
  where f' ((mClk, mRst), _) x@(iNm, ifc) = case ifcType ifc of
          Clock -> ((Just iNm, mRst), f (mClk, mRst) x)
          Reset _ -> ((mClk, Just iNm), f (mClk, mRst) x)
          _ -> ((mClk, mRst), f (mClk, mRst) x)

prettyRichModule :: RichModule -> Doc
prettyRichModule m =
  vcat $ pkgReq : modDefs : fileSetDefs : ifcsDefs
  where
    -- TCL package require
    pkgReq = text "package require qsys"
    -- Module level definitions
    modDefs = vcat [ comment (text "module:" <+> text m.name)
                   , mProp "NAME" m.name
                   , mProp "DISPLAY_NAME" m.name ]
    -- File Sets definitions
    fileSetNm = m.name ++ "_fileset"
    fileSetDefs = case m.topFile of
      Just f -> vcat [ comment $ text "file set"
                     , hsep [ text "add_fileset"
                            , text fileSetNm
                            , text "QUARTUS_SYNTH" ]
                     , hsep [ text "set_fileset_property"
                            , text fileSetNm
                            , text "TOP_LEVEL"
                            , text m.name ]
                     , hsep [ text "add_fileset_file"
                            , text $ takeFileName f
                            , text "VERILOG"
                            , text "PATH"
                            , text f
                            , text "TOP_LEVEL_FILE" ] ]
      _ -> empty
    -- Sub-Interface level definitions
    ifcsDefs = scanlWithClkRst ifcDefs (M.toList m.ifcs)
    ifcDefs (mClk, mRst) (iNm, ifc@(Ifc ps)) | ifcType ifc == Ignore = empty
                                             | otherwise =
      vcat $ [ comment (text "interface:" <+> text iNm)
             , iAdd iNm ifc
             , iProp iNm "ENABLED" "true"
             , case (ifcType ifc, ifcClock ifc, mClk) of
                 (Clock, _, _) -> empty
                 (_, Just clk, _) -> iAssocClk iNm clk
                 (_, _, Just clk) -> iAssocClk iNm clk
                 _ -> empty
             , case (ifcType ifc, ifcReset ifc, mRst) of
                 (Clock, _, _) -> empty
                 (Reset _, _, _) -> empty
                 (_, Just rst, _) -> iAssocRst iNm rst
                 (_, _, Just rst) -> iAssocRst iNm rst
                 _ -> empty
             , case ifcType ifc of
                 Reset n -> iRstPolarity iNm n
                 _ -> empty ] ++ fmap (iIfcPort iNm) ps
    -- query helpers
    getClk ident = case M.lookup ident m.ifcs of Just (Ifc (p:_)) -> Just p
                                                 _ -> Nothing
    getRst ident = case M.lookup ident m.ifcs of Just (Ifc (p:_)) -> Just p
                                                 _ -> Nothing
    -- Quartus platform designer command helpers
    iAssocClk iNm clkIdent = case getClk clkIdent of
      Just clk | clk.width == 1 ->
        iProp iNm "associatedClock" clk.identifier
      _ -> error $ "broken clock: " ++ show clkIdent
    iAssocRst iNm rstIdent = case getRst rstIdent of
      Just rst | rst.width == 1 ->
        iProp iNm "associatedReset" rst.identifier
      _ -> error $ "broken reset: " ++ show rstIdent
    iRstPolarity iNm n =
      iProp iNm "synchronousEdges" $ if n then "DEASSERT" else "ASSERT"
    iPortSig p = case p.typeIfc of
      Clock -> "clk"
      Reset n -> "reset" ++ if n then "_n" else ""
      Irq -> "irq"
      _ -> p.identSig
    iIfcPort iNm p =
      iPort iNm p.rawIdentifier (iPortSig p) (show p.direction) p.width
    -- generic Quartus platform designer command helpers
    mProp nm val = hsep [ text "set_module_property", text nm, text val ]
    iAdd nm ifc =
      hsep [ text "add_interface"
           , text nm
           , text (show . ifcType $ ifc)
           , text (showIfcDirection ifc)
           ]
    iProp iNm pNm val =
      hsep [ text "set_interface_property", text iNm, text pNm, text val ]
    iPort iNm pNm sNm dir w = hsep [ text "add_interface_port"
                                   , text iNm, text pNm, text sNm
                                   , text dir, integer w ]

pretty_QUARTUS_IP_TCL :: RichModule -> String
pretty_QUARTUS_IP_TCL = render . prettyRichModule
