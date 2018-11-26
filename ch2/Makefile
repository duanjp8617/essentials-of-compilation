OBJS = r1.cmo  main.cmo

.PHONY: clean
prog: $(OBJS)
	@echo Linking $@
	ocamlc -o $@ -I `camlp5 -where` gramlib.cma $(OBJS)

r1.cmi:
r1.cmo: r1.cmi
main.cmo: r1.cmi 


%.cmi: %.mli
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $<

%.cmo: %.ml
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $< 

clean:
	rm -f *.cmo *.cmi
