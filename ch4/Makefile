OBJS = r2.cmo  main.cmo

.PHONY: clean
prog: $(OBJS)
	@echo Linking $@
	ocamlc -o $@ -I `camlp5 -where` gramlib.cma $(OBJS)

r2.cmi:
r2.cmo: r2.cmi
#c0.cmi: r1.cmi
#c0.cmo: c0.cmi r1.cmi
#x86_0.cmi: c0.cmi
#x86_0.cmo : x86_0.cmi c0.cmi
main.cmo: r2.cmi 


%.cmi: %.mli
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $<

%.cmo: %.ml
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $< 

clean:
	rm -f *.cmo *.cmi
