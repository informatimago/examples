 ALL_PROGRAMS=  \
	client-lisp-ccl     \
	server-lisp-ccl      \
	client-lisp-ecl       \
	server-lisp-ecl        \
	client-lisp-sbcl        \
	server-lisp-sbcl         \
	client-lisp-clisp         \
	server-lisp-clisp

all:$(ALL_PROGRAMS)

ECL_INCS=-I/opt/local/include
ECL_LIBS=-L/opt/local/lib -lecl
ECL_RUN=DYLD_LIBRARY_PATH=/opt/local/lib:$(DYLD_LIBRARY_PATH) LD_LIBRARY_PATH=/opt/local/lib:$(LD_LIBRARY_PATH)

FPC=fpc
CLISP=clisp
CCL=ccl
ECL=ecl
SBCL=sbcl
CC=cc
HASKELL=ghc
LINE="//----------------------------------------------------------------------"
HERE=$(shell pwd)

define compile
	@echo "$2" >  $1
	@$2        >> $1 2>&1 || cat $1
endef

.PHONY: all clean test

client-c:client.c
	@printf "// Generating Executable from %s source: %s\n" "C" $@
	$(call compile,client-c.log,$(CC) -o $@ client.c)

client-c-static:client.c
	@printf "// Generating Static Executable from %s source: %s\n" "C" $@
	$(call compile,client-c-static.log,$(CC) -static -o $@ client.c)

client-pascal:client.pas
	@printf "// Generating Executable from %s source: %s\n" "Pascal" $@
	$(call compile,client-pascal.log,$(FPC) -oclient-pascal client.pas)

client-lisp-ccl:generate-client.lisp generate.lisp client.asd client.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ccl-*$(HERE)
	$(call compile,client-lisp-ccl.log,$(CCL) -n < generate-client.lisp)
	-@mv client client-lisp-ccl

client-lisp-clisp:generate-client.lisp generate.lisp client.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/clisp-*$(HERE)
	$(call compile,client-lisp-clisp.log,$(CLISP) -norc < generate-client.lisp)
	-@mv client client-lisp-clisp

client-lisp-clisp-fas:Makefile client.fas client-clisp-fas-rt.fas
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	@(echo '#!/usr/local/bin/clisp -norc -ansi -q -E utf-8' ;\
	  cat client.fas client-clisp-fas-rt.fas ) > $@
	@chmod 755 $@

client-clisp-fas-rt.fas:client-clisp-fas-rt.lisp
	@printf "// Compiling: %s\n" $@
	$(call compile,client-lisp-clisp-fas-rt.log,$(CLISP) -ansi -q -E utf-8 -norc -c $^ -o $@)

client.fas:client.lisp
	@printf "// Compiling: %s\n" $@
	$(call compile,client-lisp-clisp-fas.log,$(CLISP) -ansi -q -E utf-8 -norc -c $^ -o $@)

client-lisp-sbcl:generate-client.lisp generate.lisp client.asd client.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/sbcl-*$(HERE)
	$(call compile,client-lisp-sbcl.log,$(SBCL) --no-userinit < generate-client.lisp)
	-@mv client client-lisp-sbcl

client-lisp-ecl:generate-client.lisp generate.lisp client.asd client.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ecl-*$(HERE)
	$(call compile,client-lisp-ecl.log,$(ECL) -norc < generate-client.lisp)
	-@mv client client-lisp-ecl

client-ecl-lisp:client-ecl-lisp.c client.lisp
	@printf "// Generating Object from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ecl-*$(HERE)
	$(call compile,client-ecl-lisp-lisp.log,$(ECL) --norc --eval '(compile-file "client.lisp")' --eval '(quit)')
	@printf "// Generating Executable from %s source: %s\n" "C using libecl" $@
	$(call compile,client-ecl-lisp-c.log,$(CC) -o client-ecl-lisp client-ecl-lisp.c client.fas $(ECL_INCS) $(ECL_LIBS))

# ecl-hello-r-lisp: ecl-hello-r-lisp.c ecl-hello-r-lisp.a
# 		$(CC) `ecl-config --cflags` -o $@ ecl-hello-r-lisp.c ecl-hello-r-lisp.a `ecl-config --ldflags` -lecl
# 
# ecl-hello-r-lisp.a: constants.h ecl-hello-r-lisp.lisp
# 		# HACK: Force recompilation of ecl-hello-r-lisp.lisp
# 		# when header file changes. There's likely a better
# 		# way to do this via asdf:make-build ...
# 		touch ecl-hello-r-lisp.lisp
# 		ecl -norc \
# 		-eval '(require :asdf)' \
# 		-eval '(push "./" asdf:*central-registry*)' \
# 		-eval '(asdf:make-build :ecl-hello-r-lisp :type :static-library :move-here "./" :init-name "init_lib_ECL_HELLO_R_LISP")' \
# 		-eval '(quit)'


client-ecl:client-ecl.c
	@printf "// Generating Executable from %s source: %s\n" "C using libecl" $@
	$(call compile,client-ecl-c.log,$(CC) -o client-ecl client-ecl.c $(ECL_INCS) $(ECL_LIBS))

client-haskell:client.hs
	@printf "// Generating Executable from %s source: %s\n" "Haskell" $@
	-@rm -f *.o
	$(call compile,client-haskell.log,$(HASKELL) $^)
	-@mv client client-haskell


server-c:server.c
	@printf "// Generating Executable from %s source: %s\n" "C" $@
	$(call compile,server-c.log,$(CC) -o $@ server.c)

server-c-static:server.c
	@printf "// Generating Static Executable from %s source: %s\n" "C" $@
	$(call compile,server-c-static.log,$(CC) -static -o $@ server.c)

server-pascal:server.pas
	@printf "// Generating Executable from %s source: %s\n" "Pascal" $@
	$(call compile,server-pascal.log,$(FPC) -oserver-pascal server.pas)

server-lisp-ccl:generate-server.lisp generate.lisp server.asd server.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ccl-*$(HERE)
	$(call compile,server-lisp-ccl.log,$(CCL) -n < generate-server.lisp)
	-@mv server server-lisp-ccl

server-lisp-clisp:generate-server.lisp generate.lisp server.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/clisp-*$(HERE)
	$(call compile,server-lisp-clisp.log,$(CLISP) -norc < generate-server.lisp)
	-@mv server server-lisp-clisp

server-lisp-clisp-fas:Makefile server.fas server-clisp-fas-rt.fas
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	@(echo '#!/usr/local/bin/clisp -norc -ansi -q -E utf-8' ;\
	  cat server.fas server-clisp-fas-rt.fas ) > $@
	@chmod 755 $@

server-clisp-fas-rt.fas:server-clisp-fas-rt.lisp
	@printf "// Compiling: %s\n" $@
	$(call compile,server-lisp-clisp-fas-rt.log,$(CLISP) -ansi -q -E utf-8 -norc -c $^ -o $@)

server.fas:server.lisp
	@printf "// Compiling: %s\n" $@
	$(call compile,server-lisp-clisp-fas.log,$(CLISP) -ansi -q -E utf-8 -norc -c $^ -o $@)

server-lisp-sbcl:generate-server.lisp generate.lisp server.asd server.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/sbcl-*$(HERE)
	$(call compile,server-lisp-sbcl.log,$(SBCL) --no-userinit < generate-server.lisp)
	-@mv server server-lisp-sbcl

server-lisp-ecl:generate-server.lisp generate.lisp server.asd server.lisp
	@printf "// Generating Executable from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ecl-*$(HERE)
	$(call compile,server-lisp-ecl.log,$(ECL) -norc < generate-server.lisp)
	-@mv server server-lisp-ecl

server-ecl-lisp:server-ecl-lisp.c server.lisp
	@printf "// Generating Object from %s source: %s\n" "Lisp" $@
	-@rm -rf ~/.cache/common-lisp/ecl-*$(HERE)
	$(call compile,server-ecl-lisp-lisp.log,$(ECL) --norc --eval '(compile-file "server.lisp")' --eval '(quit)')
	@printf "// Generating Executable from %s source: %s\n" "C using libecl" $@
	$(call compile,server-ecl-lisp-c.log,$(CC) -o server-ecl-lisp server-ecl-lisp.c server.fas $(ECL_INCS) $(ECL_LIBS))

# ecl-hello-r-lisp: ecl-hello-r-lisp.c ecl-hello-r-lisp.a
# 		$(CC) `ecl-config --cflags` -o $@ ecl-hello-r-lisp.c ecl-hello-r-lisp.a `ecl-config --ldflags` -lecl
# 
# ecl-hello-r-lisp.a: constants.h ecl-hello-r-lisp.lisp
# 		# HACK: Force recompilation of ecl-hello-r-lisp.lisp
# 		# when header file changes. There's likely a better
# 		# way to do this via asdf:make-build ...
# 		touch ecl-hello-r-lisp.lisp
# 		ecl -norc \
# 		-eval '(require :asdf)' \
# 		-eval '(push "./" asdf:*central-registry*)' \
# 		-eval '(asdf:make-build :ecl-hello-r-lisp :type :static-library :move-here "./" :init-name "init_lib_ECL_HELLO_R_LISP")' \
# 		-eval '(quit)'


server-ecl:server-ecl.c
	@printf "// Generating Executable from %s source: %s\n" "C using libecl" $@
	$(call compile,server-ecl-c.log,$(CC) -o server-ecl server-ecl.c $(ECL_INCS) $(ECL_LIBS))

server-haskell:server.hs
	@printf "// Generating Executable from %s source: %s\n" "Haskell" $@
	-@rm -f *.o
	$(call compile,server-haskell.log,$(HASKELL) $^)
	-@mv server server-haskell



test:$(ALL_PROGRAMS)
	@for p in $(ALL_PROGRAMS) ; do printf "%-20s: %s\n" "$$p"  "$$($(ECL_RUN) ./$$p)" ; done
	@ls -l $(ALL_PROGRAMS)

clean:
	-rm -f *.o *.fas *.lib *.log *.hi
	-rm -f $(ALL_PROGRAMS)
