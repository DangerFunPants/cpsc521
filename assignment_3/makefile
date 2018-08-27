BNFC_DIR = ./bnfc
BNFC = bnfc
MAKE = make
HFMT = hfmt
CF_FILE = ExprGrammar.cf
CC = ghc
CFLAGS=-XTemplateHaskell
SRC_DIR = ./src
BNFC_SOURCES = $(BNFC_DIR)/AbsExprGrammar.hs $(BNFC_DIR)/ErrM.hs $(BNFC_DIR)/LexExprGrammar.hs $(BNFC_DIR)/ParExprGrammar.hs $(BNFC_DIR)/PrintExprGrammar.hs $(BNFC_DIR)/SkelExprGrammar.hs
TEST_EXE=TestExprGrammar
BIN=a2

a2 : $(BNFC_DIR)/$(CF_FILE) $(SRC_DIR)/*.hs $(TEST_EXE)
	$(CC) $(CFLAGS) $(SRC_DIR)/*.hs $(BNFC_SOURCES) -o $(BIN)

TestExprGrammar : $(BNFC_DIR)/$(CF_FILE)
	$(BNFC) -haskell -m $(BNFC_DIR)/$(CF_FILE) -o $(BNFC_DIR)
	$(MAKE) -C $(BNFC_DIR)
	mv $(BNFC_DIR)/$(TEST_EXE) ./

pretty : $(SRC_DIR)/*.hs
	$(HFMT) -w $(SRC_DIR)/*.hs

clean : 
	rm -f $(TEST_EXE)
	rm -f $(BIN)
	rm -f $(SRC_DIR)/*.hi $(SRC_DIR)/*.o
	rm -f $(SRC_DIR)/*.dyn_*
	$(MAKE) distclean -C $(BNFC_DIR)
	

