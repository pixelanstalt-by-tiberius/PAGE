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
	bin/debug/tools/irc -fres/fez.png -fres/p.png -fres/a.png -fres/g.png -fres/e.png -fres/page.png -ores/splash_res.pas
	# compile resource-pas for manaspace bitmap font
	bin/debug/tools/irc -fres/fonts/manaspace16.png -bres/fonts/manaspace16.fnt -ores/fonts/font_manaspace16.pas
	bin/debug/tools/irc -fres/fonts/manaspace8.png -bres/fonts/manaspace8.fnt -ores/fonts/font_manaspace8.pas
	# compile resource-pas for dogica bitmap font
	bin/debug/tools/irc -fres/fonts/dogica8.png -bres/fonts/dogica8.fnt -ores/fonts/font_dogica8.pas

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
	rm -rf res/fonts/*.pas
