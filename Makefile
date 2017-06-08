GHC_FLAGS=

all: interpreter

run: interpreter example.cmm
	cat example.cmm | ./interpreter

LexCmm.x:
	bnfc Cmm.cf
	happy -gca ParCmm.y
	alex -g LexCmm.x

interpreter: LexCmm.x Interpret.hs Interpreter.hs Exec.hs
	ghc --make $(GHC_FLAGS) Interpret.hs -o interpreter

test: interpreter
	echo "testing good/"
	for f in good/*; do ./interpreter < $$f; done

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi interpreter

distclean: clean
	-rm -f DocCmm.* LexCmm.* ParCmm.* LayoutCmm.* SkelCmm.* PrintCmm.* TestCmm.* AbsCmm.* TestCmm ErrM.* SharedString.* ComposOp.* cmm.dtd XMLCmm.*
	
.PHONY: all run clean distclean
