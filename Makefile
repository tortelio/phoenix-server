PROJECT_NAME := phoenix

TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))

ERL := $(shell which erl)
REBAR := $(shell which rebar)
RELX := $(shell which relx)

CONSOLE_ERL_FLAGS := -pa $(TOP_DIR)../phoenix-server/ebin -pa $(TOP_DIR)deps/*/ebin -config $(TOP_DIR)$(PROJECT_NAME).config -args_file $(TOP_DIR)vm.args

_rel/$(PROJECT_NAME)/bin/$(PROJECT_NAME): $(RELX) compile
	$(RELX)

.PHONY: all deps compile clean console release

all: deps compile

deps: $(REBAR)
	$(REBAR) get-deps
	$(REBAR) -r update-deps
	$(REBAR) compile

compile: $(REBAR)
	$(REBAR) skip_deps=true compile

console: compile $(ERL)
	$(ERL) $(CONSOLE_ERL_FLAGS) -s $(PROJECT_NAME)

release: _rel/$(PROJECT_NAME)/bin/$(PROJECT_NAME)

clean: $(REBAR)
	$(REBAR) clean
