ifeq ($(OS), Windows_NT)
	libpre=
	libext=dll
	exesuffix=.exe
else
	libpre=lib
	libext=so
	exesuffix=
endif

release: lazflags = -r -B --bm=Release -q
debug: lazflags = --bm=Debug -q -q

release: resources engine starter
debug: resources engine starter
debug_tests: debug tests do_run_tests

resource_creator:
	lazbuild src/tools/InlineResourceCreator/irc.lpi

resources: resource_creator
	# compile resource-pas for the splashscreen
	bin/debug/tools/irc -fres/splash_1.png -fres/splash_2.png -ores/splash_res.pas

engine:
	lazbuild $(lazflags) src/engine/PAGE.lpi
engine_test:
	lazbuild -q -q tests/engine/engine_test.lpi

starter:
	lazbuild $(lazflags) src/starter/load.lpi

tests: engine_test
do_run_tests:
	cd bin/debug/tests; ./engine_test$(exesuffix)

.PHONY: clean
clean:
	@echo $(OS)
	rm -f bin/debug/$(libpre)page.$(libext)
	rm -f bin/debug/page_load$(exesuffix)
	rm -f bin/release/$(libpre)page.$(libext)
	rm -f bin/release/page_load$(exesuffix)
	rm -f bin/debug/tests/*
	rm -f bin/debug/tools/*
	rm -rf src/engine/lib
	rm -rf src/starter/lib
	rm -rf tests/engine/lib
	rm -rf src/tools/InlineResourceCreator/lib
	rm -rf res/*.pas
