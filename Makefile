include vsn.mk

APP_NAME=erlang_media_server
PFX=ems
VSN=$(ERLANG_MEDIA_SERVER_VSN)

ESRC=./src
EBIN=./ebin
ESUBDIRS=rtp

SRCFILES= $(wildcard $(ESRC)/*.erl) $(foreach dir, $(ESUBDIRS), $(wildcard $(ESRC)/$(dir)/*.erl))
MODULES=$(subst .erl, , $(subst $(ESRC)/, , $(SRCFILES)))
MODULES_BASE=$(foreach mod, $(MODULES), $($(notdir mod)))
MODULES_COMMA=$(foreach mod, $(MODULES), $($(notdir mod)),)

HRL_FILES=
INTERNAL_HRL_FILES= $(APP_NAME).hrl
ERL_FILES= $(MODULES:%=%.erl)
DOC_FILES=$(ERL_FILES)
ERLC=erlc
EMULATOR=beam
BEAMS= $(foreach module, $(MODULES), $(EBIN)/$(module).beam)
ERL_COMPILE_FLAGS += -I./include +debug_info
APP_TARGET=$(EBIN)/$(APP_NAME).app
APP_SRC=$(ESRC)/$(APP_NAME).app.src

#test:
#	@echo $(BEAMS)

all: $(EBIN) $(BEAMS) $(APP_TARGET)

debug: all
	erl -pa ebin ebin/rtp -smp auto -run debugger -run ems 
	# -run appmon 
	# -noshell
	
# Note: In the open-source build, clean must not destroy the preloaded
# beam files.
clean:
	rm -f $(TARGET_FILES)
	rm -f core
	rm -rf $(EBIN)
	rm -rf *html
	rm -f erl_crash.dump
	rm -f *~*
	rm -f $(APP_NAME).includes

$(EBIN):
	mkdir $(EBIN) $(foreach dir, $(ESUBDIRS), $(EBIN)/$(dir))

$(EBIN)/%.beam: $(ESRC)/%.erl 
	@$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(dir $(patsubst src/%.erl, ebin/%.beam, $<)) $<
		
$(APP_TARGET): $(APP_SRC) vsn.mk $(BEAMS)
		sed -e 's;%VSN%;$(VSN);' \
			-e 's;%PFX%;$(PFX);' \
			-e 's;%APP_NAME%;$(APP_NAME);' \
			-e 's;%MODULES_BASE%;%MODULES_BASE%$(MODULES_COMMA);' \
			$< > $<".tmp" 
		sed -e 's/%MODULES%\(.*\),/\1/' \
			$<".tmp" > $@ 
		rm $<".tmp"

$(APPUP_TARGET): $(APPUP_SRC) vsn.mk
		sed -e 's;%VSN%;$(VSN);' $< > $@