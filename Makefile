GCC=g++
CFLAGS=-std=c++11 -g
LDFLAGS=-lstdc++
BIN=bin
SRC=src

SEMANTIC_ANALYZER_HEADERS=$(SRC)/SemanticAnalyzer/SymbolTable.hpp

COMMON_HEADERS=$(SRC)/common/Symbol.hpp $(SRC)/common/TokenPair.hpp

PARSER_HEADERS=$(SRC)/Parser/SyntaxTree.hpp

SCANNER_HEADERS=$(SRC)/Scanner/Scanner.hpp

HEADERS=$(COMMON_HEADERS) $(SEMANTIC_ANALYZER_HEADERS) $(PARSER_HEADERS) \
		$(SCANNER_HEADERS) $(SRC)/IrCodeGenerator.hpp \
		$(SRC)/MipsCodeGenerator.hpp $(SRC)/RegisterAllocator.hpp \
		$(SRC)/CFGGenerator.hpp

.SUFFIXES:

all: parser scanner

scanner.o: $(SRC)/Scanner/Scanner.cpp $(HEADERS)
	$(GCC) $(CFLAGS) -c -o $@  $<

$(BIN)/scanner: scanner.o
	mkdir -p bin
	$(GCC) -o $@  $^ $(LDFLAGS)

scanner: $(BIN)/scanner

gen-files/generated_symbol_to_action.inc: $(SRC)/Parser/gen_symbol_to_action.cpp \
		$(HEADERS)
	mkdir -p gen-files
	$(GCC) $(CFLAGS) -o gen-files/gen_symbol_to_action $<
	gen-files/gen_symbol_to_action $@

ir_code_generator.o: $(SRC)/IrCodeGenerator.cpp $(HEADERS)
	$(GCC) $(CFLAGS) -c -o $@ $<

mips_code_generator.o: $(SRC)/MipsCodeGenerator.cpp $(HEADERS)
	$(GCC) $(CFLAGS) -c -o $@ $<

register_allocator.o: $(SRC)/RegisterAllocator.cpp $(HEADERS)
	$(GCC) $(CFLAGS) -c -o $@ $<

symbol_table.o: $(SRC)/SemanticAnalyzer/SymbolTable.cpp $(HEADERS)
	$(GCC) $(CFLAGS) -c -o $@ $<

syntax_tree.o: $(SRC)/Parser/SyntaxTree.cpp $(HEADERS)
	$(GCC) $(CFLAGS) -c -o $@ $<

parser.o: $(SRC)/Parser/Parser.cpp $(HEADERS) \
		gen-files/generated_symbol_to_action.inc
	$(GCC) $(CFLAGS) -c -o $@ $<

$(BIN)/parser: parser.o ir_code_generator.o symbol_table.o syntax_tree.o \
		mips_code_generator.o register_allocator.o symbol_table.o syntax_tree.o
	mkdir -p bin
	$(GCC) -o $@ $^ $(LDFLAGS)

parser: $(BIN)/parser

clean:
	rm -rf $(BIN)/parser $(BIN)/scanner gen-files *.o

TEST_INPUTS:=$(wildcard testCases/test-phaseI/*.tiger)

gen_test_output: parser
	-for src_file in $(TEST_INPUTS); do \
	  bin/parser "$${src_file}" -d > "$${src_file%.tiger}.out.d" 2>&1; \
		bin/parser "$${src_file}" > "$${src_file%.tiger}.out" 2>&1; \
		mv "$${src_file}.ir" "$${src_file}.expected"; \
	done

test: parser
	for src_file in $(TEST_INPUTS); do \
	  bin/parser "$${src_file}" -d 2>&1 | diff "$${src_file%.tiger}.out.d" -; \
		bin/parser "$${src_file}" 2>&1 | diff "$${src_file%.tiger}.out" -; \
		if [ -f "$${src_file}.expected" ]; then diff "$${src_file}.expected" "$${src_file}.ir"; fi; \
	done

test2: parser
	for src_file in $(wildcard testCases/phase2/*.tiger); do \
		bin/parser "$${src_file}"; \
	done

report:
	mkdir -p ProjectPhase2
	git checkout-index -a -f --prefix ProjectPhase2/
	lyx -E pdf2 ProjectPhase2/report.pdf ProjectPhase2/doc/report.lyx
	rm -rf ProjectPhase2/{doc,testCases}
	tar cvf ProjectPhase2.tar ProjectPhase2
	rm -rf ProjectPhase2
