include vsn.mk

APP_NAME=ems
PFX=ems
VSN=$(ERLANG_MEDIA_SERVER_VSN)

ESRC=src
EBIN=ebin
ERLIB=/opt/local/lib/erlang
ERLC=erlc
ERL=erl
ERL_FLAGS=-smp enable -pa apps/listener/ebin -pa apps/rtsp/ebin -pa apps/ems/ebin
ERL_COMPILE_FLAGS += +debug_info +native -smp
APP_TARGET=$(EBIN)/$(APP_NAME).app
APP_SRC=$(ESRC)/$(APP_NAME).app

BEAMS=$(wildcard $(EBIN)/*.beam)
MODS=$(patsubst $(EBIN)/%.beam, %, $(BEAMS));

all:
	$(ERL) -make $(ERL_COMPILE_FLAGS)

inter: all
	erl $(ERL_FLAGS) -boot start_sasl

run: all
	erl $(ERL_FLAGS) -run ems -noshell

debug: all
	erl $(ERL_FLAGS) -boot start_sasl -run appmon -run debugger start -run ems

dialyzer: default
	dialyzer -c $(EBIN)

# Note: In the open-source build, clean must not destroy the preloaded
# beam files.
clean:
	rm -fr apps/*/ebin/*.beam
	rm -f erl_crash.dump

$(APPUP_TARGET): $(APPUP_SRC) vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(EBIN):
	mkdir $(EBIN)
