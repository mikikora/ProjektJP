
SOURCES = core.ml core.mli main.ml support.ml support.mli syntax.ml syntax.mli
OBJS = support.cmo syntax.cmo core.cmo parser.cmo lexer.cmo main.cmo

# DEPEND = lexer.ml parser.ml

all: $(DEPEND) $(OBJS) f

install: clean f

f: $(OBJS) 
	ocamlc -o $@ $(OBJS)

format:
	ls $(SOURCES) | xargs ocamlformat -i

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml %.cmi
	ocamlc -c $<

parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli parser.automaton parser.conflicts
	menhir --dump --explain --infer parser.mly

lexer.ml: lexer.mll
	@rm -f lexer.ml
	ocamllex $<

lexer.cmo: lexer.ml
	ocamlc -c $<

main.cmo: 
	ocamlc -c main.ml

clean:
	rm -rf lexer.ml parser.ml parser.mli parser.automaton parser.conflicts \
	*.o *.cmo *.cmi f *~ *.bak
