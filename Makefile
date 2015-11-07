REBAR=$(shell which rebar3)

ifeq ($(REBAR),)
$(error "Rebar3 not found on this system")
endif

.PHONY: all compile clean test

all: compile

# =============================================================================
# Rules to build the system
# =============================================================================

compile:
	$(REBAR) compile

test:
	@$(REBAR) eunit

clean:
	$(REBAR) clean

