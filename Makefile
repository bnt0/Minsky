# Locations

SRC_DIR := src
OUT_DIR := bin

# Tool

CMPLR := ghc

all: main

main:
	$(CMPLR) -i$(SRC_DIR) $(SRC_DIR)/Main.hs -o $(OUT_DIR)/main -outputdir=$(OUT_DIR)
