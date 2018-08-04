DC = haddock

DC_OPT = --html
DC_OPT += -o doc
DC_OPT += --hyperlinked-source
DC_OPT += -t "Truth Table"
DC_OPT += --ignore-all-exports
SRC = src/TruthTable/WellFormedFormula.hs
SRC += src/TruthTable/PropLogTree.hs
SRC += src/TruthTable/PropLogValuation.hs

doc : doc_dir
	$(DC) $(DC_OPT) $(SRC)

doc_dir :
	mkdir -p doc
