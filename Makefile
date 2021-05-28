#-
# Copyright (c) 2021 Alexandre Joannou
# All rights reserved.
#
# This material is based upon work supported by the DoD Information Analysis
# Center Program Management Office (DoD IAC PMO), sponsored by the Defense
# Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
# opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Air Force Installation Contracting Agency (AFICA).
#
# @BERI_LICENSE_HEADER_START@
#
# Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  BERI licenses this
# file to you under the BERI Hardware-Software License, Version 1.0 (the
# "License"); you may not use this file except in compliance with the
# License.  You may obtain a copy of the License at:
#
#   http://www.beri-open-systems.org/legal/license-1-0.txt
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.
#
# @BERI_LICENSE_HEADER_END@
#

SRC_DIR = $(CURDIR)/src
HI_DIR = $(CURDIR)/hidir
O_DIR = $(CURDIR)/odir
MAIN_SRC = $(SRC_DIR)/VIPBundle.hs
SRCS = $(MAIN_SRC)
SRCS += $(SRC_DIR)/VIPBundle/InterfaceInference.hs
SRCS += $(SRC_DIR)/VIPBundle/Parse.hs
SRCS += $(SRC_DIR)/VIPBundle/Types.hs
SRCS += $(SRC_DIR)/VIPBundle/Pretty_QUARTUS_IP_TCL.hs

.PHONY: all

all: vipbundle

vipbundle: $(SRCS)
	ghc --make -j \
        -hidir $(HI_DIR) -odir $(O_DIR) -i$(SRC_DIR) \
		-o $@ \
        $(MAIN_SRC)

.PHONY: clean mrproper

clean:
	rm -rf $(HI_DIR) $(O_DIR)

mrproper: clean
	rm -f vipbundle
