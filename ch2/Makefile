OBJS = r1.cmo c0.cmo x86_0.cmo  main.cmo

.PHONY: clean
prog: $(OBJS)
	@echo Linking $@
	ocamlc -o $@ -I `camlp5 -where` gramlib.cma $(OBJS)

r1.cmi:
r1.cmo: r1.cmi
c0.cmi: r1.cmi
c0.cmo: c0.cmi r1.cmi
x86_0.cmi: c0.cmi
x86_0.cmo : x86_0.cmi c0.cmi
main.cmo: r1.cmi c0.cmi x86_0.cmi


%.cmi: %.mli
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $<

%.cmo: %.ml
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $< 

clean:
	rm -f *.cmo *.cmi
