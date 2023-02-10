{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

{-# LANGUAGE BlockArguments #-}

module VIPBundle.Parse (
  parseVerilog
) where

import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

import VIPBundle.Types

-- minimal language definition and helpers
------------------------------------------
keywords = [ "module", "endmodule", "input", "output" ]
identifierStart = letter <|> oneOf "_.$\\"
tp = makeTokenParser $ emptyDef { commentLine   = "//"
                                , identStart    = identifierStart
                                , identLetter   = identifierStart <|> digit
                                , reservedNames = keywords }

ws = whiteSpace tp
stripTrailWs p = p >>= \res -> ws >> return res
reservedWs str = stripTrailWs $ reserved tp str
naturalWs = fromInteger <$> stripTrailWs (natural tp)
naturalNoWs = fromInteger <$> natural tp
identifierWs = stripTrailWs $ Text.Parsec.Token.identifier tp
parensWs p = stripTrailWs (parens tp $ whiteSpace tp >> p)
bracesWs p = stripTrailWs (braces tp $ whiteSpace tp >> p)
bracketsWs p = stripTrailWs (brackets tp $ whiteSpace tp >> p)
charWs c = stripTrailWs $ char c
stringWs c = stripTrailWs $ string c

kw = reservedWs
nat = naturalWs
natNoWs = naturalNoWs
ident = identifierWs

commaLst :: Parser x -> Parser [x]
commaLst p = sepBy p $ charWs ','

commaLst1 :: Parser x -> Parser [x]
commaLst1 p = sepBy1 p $ charWs ','

skipTill :: Parser x -> Parser x
skipTill p = p <|> (anyChar >> skipTill p)

-- parse verilog module interfaces
----------------------------------
parseVerilogModule :: Parser VerilogModule
parseVerilogModule = do
  kw "module"
  nm <- ident
  decls <- parensWs $ commaLst1 ident
  semi tp
  ports <- harvestPorts decls []
  return $ VerilogModule nm $ reverse ports
  where harvestPorts decls acc =
              (kw "endmodule" >> return acc)
          <|> choice [
                try (parsePortDef decls) >>= \x -> harvestPorts decls $ x : acc
              , anyChar >> harvestPorts decls acc ]

parsePortDef :: [String] -> Parser VerilogPort
parsePortDef decls = do
  dir <- choice [ kw "input" >> return In
                , kw "output" >> return Out ]
  w <- option 1 parseWidthFromSlice
  nm <- ident
  semi tp
  if nm `elem` decls then return $ VerilogPort nm dir w else parserZero

parseWidthFromSlice :: Parser Integer
parseWidthFromSlice = bracketsWs do
  hi <- naturalWs
  colon tp >> ws
  lo <- naturalWs
  return if hi > lo then hi - lo + 1 else 0

parseAll :: Parser [VerilogModule]
parseAll = do
  res <- many1 $ skipTill parseVerilogModule
  skipTill eof
  return res

parseVerilog :: Maybe FilePath -> String -> [VerilogModule]
parseVerilog mfp src = case parse parseAll fp src of
  Left  e -> error $ fp ++ ": " ++ show e ++ "\n"
  Right x -> x
  where fp = fromMaybe "stdin" mfp
