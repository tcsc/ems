include vsn.mk

APP_NAME=ems
PFX=ems
VSN=$(ERLANG_MEDIA_SERVER_VSN)

ESRC=./src
EBIN=./ebin
ERLIB=/opt/local/lib/erlang
ERLC=erlc
ERL=erl
ERL_FLAGS=-pz $(ERLIB)/erlang_js/ebin -pa ebin -boot start_sasl
ERL_COMPILE_FLAGS += +debug_info
APP_TARGET=$(EBIN)/$(APP_NAME).app
APP_SRC=$(ESRC)/$(APP_NAME).app

BEAMS=$(wildcard $(EBIN)/*.beam)
MODS=$(patsubst $(EBIN)/%.beam, %, $(BEAMS));

all: $(APP_TARGET)
	$(ERL) -make $(ERL_COMPILE_FLAGS)

run: all
	erl $(ERL_FLAGS) -run ems 

debug: all
	erl $(ERL_FLAGS) -run ems
	# -run debugger
	# 
	# -noshell

dialyzer: default
	dialyzer -c $(EBIN)

# Note: In the open-source build, clean must not destroy the preloaded
# beam files.
clean:
	rm -fr $(EBIN)
	rm -f erl_crash.dump

$(APP_TARGET): $(EBIN)
	cp $(APP_SRC) $(APP_TARGET)

$(APPUP_TARGET): $(APPUP_SRC) vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(EBIN):
	mkdir $(EBIN)