{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
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

module VIPBundle.Types (
  PortDir (.., Start, End
             , Source, Sink
             , Master, Slave
             , Sender, Receiver)
, IfcType (..)
, VerilogPort (..)
, VerilogModule (..)
, RichPort (..)
, IfcIdentifier
, SignalIdentifier
, Ifc (..)
, ifcDirection
, ifcType
, ifcIdent
, ifcClock
, ifcReset
, showIfcDirection
, RichModule (..)
) where

import Prelude hiding ((<>))
import Data.Map qualified as M
import Text.PrettyPrint
import Data.Aeson
import GHC.Generics

-- Port Direction
--------------------------------------------------------------------------------

data PortDir = In | Out deriving (Eq, Generic)

showDirInOut :: PortDir -> String
showDirInOut  In =  "Input"
showDirInOut Out = "Output"

instance Show PortDir where show = showDirInOut

instance ToJSON PortDir

pattern Start :: PortDir
pattern Start = Out

pattern End :: PortDir
pattern End = In

showDirStartEnd :: PortDir -> String
showDirStartEnd Start = "start"
showDirStartEnd   End =   "end"

pattern Source :: PortDir
pattern Source = Out

pattern Sink :: PortDir
pattern Sink = In

showDirSourceSink :: PortDir -> String
showDirSourceSink Source = "source"
showDirSourceSink   Sink =   "sink"

pattern Master :: PortDir
pattern Master = Out

pattern Slave :: PortDir
pattern Slave = In

showDirMasterSlave :: PortDir -> String
showDirMasterSlave Master = "master"
showDirMasterSlave  Slave =  "slave"

pattern Sender :: PortDir
pattern Sender = Out

pattern Receiver :: PortDir
pattern Receiver = In

showDirSenderReceiver :: PortDir -> String
showDirSenderReceiver   Sender =   "sender"
showDirSenderReceiver Receiver = "receiver"

-- Verilog types
--------------------------------------------------------------------------------

data VerilogPort = VerilogPort {
    identifier :: String
  , direction  :: PortDir
  , width      :: Integer
  } deriving (Generic)

docVerilogPort :: VerilogPort -> Doc
docVerilogPort p =
  text p.identifier <+>
  (braces . sep . punctuate comma)
    [ text "width:" <+> integer p.width
    , text "dir:"   <+> text (show p.direction) ]

instance Show VerilogPort where show = render . docVerilogPort

instance ToJSON VerilogPort

data VerilogModule = VerilogModule {
    name  :: String
  , ports :: [VerilogPort]
  } deriving (Generic)

docVerilogModule :: VerilogModule -> Doc
docVerilogModule m =
  hang (text m.name <> colon) 2 (sep $ fmap docVerilogPort m.ports)

instance Show VerilogModule where show = render . docVerilogModule

instance ToJSON VerilogModule

-- Interface types
--------------------------------------------------------------------------------

type IfcIdentifier = String

type SignalIdentifier = String

data IfcType =
    Clock
  | Reset { activeLow :: Bool}
  | AXI4
  | AXI4Lite
  | AXI4Stream
  | Avalon
  | Irq
  | Conduit
  | Ignore
  deriving (Eq, Generic)

showIfcType :: IfcType -> String
showIfcType Clock = "clock"
showIfcType Reset {} = "reset"
showIfcType AXI4 = "axi4"
showIfcType AXI4Lite = "axi4lite"
showIfcType AXI4Stream = "axi4stream"
showIfcType Avalon = "avalon"
showIfcType Irq = "interrupt"
showIfcType Conduit = "conduit"
showIfcType Ignore = "ignored"

instance Show IfcType where show = showIfcType

instance ToJSON IfcType

data RichPort = RichPort {
    rawIdentifier :: String
  , identifier :: String
  , direction :: PortDir
  , width :: Integer
  , typeIfc :: IfcType
  , identIfc :: IfcIdentifier
  , clockIfc :: Maybe IfcIdentifier
  , resetIfc :: Maybe IfcIdentifier
  , identSig :: SignalIdentifier
  } deriving (Generic)

docRichPort :: RichPort -> Doc
docRichPort p =
  hsep [ text $ showIfcType p.typeIfc
       , text $ showDirSourceSink p.direction
       , text "--"
       , int (fromInteger p.width) <> text "-bit"
       , text "rawIdentifier:" <+> text p.rawIdentifier
       , text "identifier:" <+> text p.identifier
       , text "interface:" <+> text p.identIfc
       , text "signal:" <+> text p.identSig
       , docClk
       , docRst
       ]
  where docClk = maybe empty (\x -> text "clocked_by:" <+> text x) p.clockIfc
        docRst = maybe empty (\x -> text "reset_by:" <+> text x) p.resetIfc

instance Show RichPort where show = render . docRichPort

instance ToJSON RichPort

newtype Ifc = Ifc [RichPort] deriving (Generic)

ifcDirection :: Ifc -> PortDir
ifcDirection (Ifc []) = error "ifcDirection called on empty interface"
ifcDirection (Ifc ps) = if ins > outs then In else Out
  where (ins, outs) = foldl (\(x, y) p -> case p.direction of
                                            In -> (x+1,y)
                                            _  -> (x, y+1))
                            (0,0)
                            ps

ifcType :: Ifc -> IfcType
ifcType (Ifc []) = error "ifcType called on empty interface"
ifcType (Ifc (p:_)) = p.typeIfc

ifcIdent :: Ifc -> IfcIdentifier
ifcIdent (Ifc []) = error "ifcIdent called on empty interface"
ifcIdent (Ifc (p:_)) = p.identIfc

ifcClock :: Ifc -> Maybe IfcIdentifier
ifcClock (Ifc []) = error "ifcClock called on empty interface"
ifcClock (Ifc (p:_)) = p.clockIfc

ifcReset :: Ifc -> Maybe IfcIdentifier
ifcReset (Ifc []) = error "ifcReset called on empty interface"
ifcReset (Ifc (p:_)) = p.resetIfc

showIfcDirection :: Ifc -> String
showIfcDirection ifc = go . ifcDirection $ ifc
  where go = case ifcType ifc of Clock -> showDirSourceSink
                                 Reset _ -> showDirSourceSink
                                 AXI4 -> showDirMasterSlave
                                 AXI4Lite -> showDirMasterSlave
                                 AXI4Stream -> showDirMasterSlave
                                 Avalon -> showDirMasterSlave
                                 Irq -> showDirSenderReceiver
                                 Conduit -> showDirStartEnd
                                 Ignore -> showDirStartEnd

docIfc :: Ifc -> Doc
docIfc ifc@(Ifc ps) =
  hang (    (text . show . ifcType $ ifc)
        <+> (parens . text . showIfcDirection $ ifc) )
       2
       (vcat [ case ifcClock ifc of
                 Just clk -> text "associated clock:" <+> text clk
                 Nothing -> text "no associated clock"
             , case ifcReset ifc of
                 Just rst -> text "associated reset:" <+> text rst
                 Nothing -> text "no associated reset"
             , sep $ text "ports:" : fmap docRichPort ps ])

instance Show Ifc where show = render . docIfc

instance ToJSON Ifc

data RichModule = RichModule {
    name    :: String
  , ifcs    :: M.Map IfcIdentifier Ifc
  , topFile :: Maybe FilePath
  } deriving (Generic)

docRichModule :: RichModule -> Doc
docRichModule m =
  hang (hsep [text "--", text m.name, text "(module)", text "--"]) 2
       (vcat $ topfileline : fmap prettyIfc (M.toList m.ifcs))
  where prettyIfc (nm, ifc) =
          text "*" <+> text nm <> colon <+> docIfc ifc
        topfileline = case m.topFile of
                        Just f -> text "top file: " <> text f
                        _ -> empty

instance Show RichModule where show = render . docRichModule

instance ToJSON RichModule
