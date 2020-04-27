PYTHON=python3
INT_DRIVER_GENERATOR=$(CURDIR)/drivers/types/gen_int_driver.py
INT_DRIVERS=UInt4.drv UInt8.drv UInt160.drv UInt256.drv
OCAML_SOURCES=evm.ml common.ml
OCAML_TARGETS=$(patsubst %.ml,%.cmo,$(OCAML_SOURCES))
OCAML_PACKAGES=num,yojson

default : evm.ml

drivers: $(addprefix $(CURDIR)/drivers/types/, $(INT_DRIVERS))

$(CURDIR)/drivers/types/UInt%.drv: $(INT_DRIVER_GENERATOR)
	$(PYTHON) $(INT_DRIVER_GENERATOR) $* > $@


evm.ml: $(CURDIR)/src/MinimalEVM.mlw drivers $(CURDIR)/src/ocaml/dependency.ml
	cp $(CURDIR)/src/ocaml/dependency.ml $@
	why3 extract MinimalEVM.EVM -L src -L lib \
		$(addprefix -D $(CURDIR)/drivers/types/, $(INT_DRIVERS)) \
		-D $(CURDIR)/drivers/ocaml64.drv \
		-D $(CURDIR)/drivers/types/int.drv \
		>> $@

%.cmo: %.ml
	ocamlfind ocamlc -package $(OCAML_PACKAGES) -c $< -o $@

server: $(OCAML_TARGETS) server.cmo
	ocamlfind ocamlc -linkpkg -package $(OCAML_PACKAGES) $^ -o $@

run: evm
	./evm

clean:
	rm -f $(addprefix $(CURDIR)/drivers/types/, $(INT_DRIVERS))
	rm -f evm.ml.in
	rm -f evm server
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.cmx
	rm -f *.o
