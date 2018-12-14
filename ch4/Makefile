OBJS = r2.cmo c1.cmo x86_1.cmo  main.cmo

.PHONY: clean
prog: $(OBJS)
	@echo Linking $@
	ocamlc -o $@ -I `camlp5 -where` gramlib.cma $(OBJS)

r2.cmi:
r2.cmo: r2.cmi
c1.cmi: r2.cmi
c1.cmo: c1.cmi r2.cmi
x86_1.cmi: r2.cmi c1.cmi
x86_1.cmo : x86_1.cmi r2.cmi c1.cmi
main.cmo: r2.cmi c1.cmi x86_1.cmi


%.cmi: %.mli
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $<

%.cmo: %.ml
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $< 

clean:
	rm -f *.cmo *.cmi
