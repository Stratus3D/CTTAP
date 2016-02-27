PROJECT = cttap

.PHONY: example_tests

include erlang.mk

EXAMPLE_CT_SUITES ?= $(sort $(subst _SUITE.erl,,$(shell find example_tests -type f -name \*_SUITE.erl -exec basename {} \;)))
EXAMPLE_CT_OPTS= -ct_hooks cttap "[{filename, \"../test.tap\"}]" -logdir logs/example_cttap

build-example-ct-suites: build-ct-deps
	$(gen_verbose) erlc -v $(TEST_ERLC_OPTS) -I include/ -o example_tests/ \
		$(wildcard example_tests/*.erl example_tests/*/*.erl) -pa ebin/

EXAMPLE_CT_RUN = ct_run \
	-no_auto_compile \
	-noshell \
	-pa $(realpath ebin) $(DEPS_DIR)/*/ebin \
	-dir example_tests \
	-logdir logs

# example_tests runs all the test suites in example_tests/. We just need to
# customize a few things... (Note: this target was copied out of erlang.mk)
example_tests: ERLC_OPTS = $(TEST_ERLC_OPTS)
example_tests: CT_OPTS = $(EXAMPLE_CT_OPTS)
example_tests: CT_SUITES = $(EXAMPLE_CT_SUITES)
example_tests: CT_RUN = $(EXAMPLE_CT_RUN)
example_tests: clean deps app build-example-ct-suites
	@if [ -d "example_tests" ] ; \
	then \
		mkdir -p logs/example_cttap; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) || exit 0; \
	fi
	$(gen_verbose) rm -f test/*.beam

# Example tests need be run before the regular tests so they TAP output is available
cttap_tests: example_tests tests
