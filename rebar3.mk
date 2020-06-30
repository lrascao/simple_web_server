.PHONY: clean compile dialyzer rel run test

REBAR3_URL := https://s3.amazonaws.com/rebar3/rebar3

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR3 := $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR3 ?= $(shell which rebar3 2>/dev/null || echo "$(CURDIR)/rebar3")

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR3),)
REBAR3 := $(CURDIR)/rebar3
endif

clean: rebar3
	@$(REBAR3) clean

compile: rebar3
	@$(REBAR3) compile

dialyzer: compile
	@$(REBAR3) dialyzer

rel: rebar3
	@$(REBAR3) release

run: rebar3
	@$(REBAR3) shell

test: compile
	@$(REBAR3) ct

rebar3: $(REBAR3)

$(REBAR3):
	@echo "Retrieving $(REBAR3) from $(REBAR3_URL)"
	@curl -sLo $(REBAR3) $(REBAR3_URL) || wget $(REBAR3_URL)
	@chmod a+x $(REBAR3)
	@echo
	@echo '************************************************************************'
	@echo 'NOTE - please follow these instructions:'
	@echo
	$(CURDIR)/rebar3 local install
	@echo
	@echo '************************************************************************'
	@echo
	@rm -f $(CURDIR)/rebar3
