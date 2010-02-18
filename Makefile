include vsn.mk

APP_NAME=erlang_media_server
PFX=ems
VSN=$(ERLANG_MEDIA_SERVER_VSN)

ESRC=./src
EBIN=./ebin

ERLC=erlc
ERL=erl
ERL_COMPILE_FLAGS += +debug_info
APP_TARGET=$(EBIN)/$(APP_NAME).app
APP_SRC=$(ESRC)/$(APP_NAME).app.src

all: $(APP_TARGET)
	$(ERL) -make $(ERL_COMPILE_FLAGS)
	
run: all
	erl -pa ebin -run ems

debug: all
	erl -pa ebin -run ems debug debugger.State
	# -run appmon 
	# -noshell
	
dialyzer: all
	dialyzer -c $(EBIN)
	
# Note: In the open-source build, clean must not destroy the preloaded
# beam files.
clean:
	rm -f $(EBIN)/*.beam
	rm -f erl_crash.dump
			
$(APP_TARGET): $(APP_SRC) vsn.mk $(BEAMS)
		sed -e 's;%VSN%;$(VSN);' \
			-e 's;%PFX%;$(PFX);' \
			-e 's;%APP_NAME%;$(APP_NAME);' \
			-e 's;%MODULES%;%MODULES%$(MODULES_COMMA);' \
			$< > $<".tmp" 
		sed -e 's/%MODULES%\(.*\),/\1/' \
			$<".tmp" > $@ 
		rm $<".tmp"

$(APPUP_TARGET): $(APPUP_SRC) vsn.mk
		sed -e 's;%VSN%;$(VSN);' $< > $@