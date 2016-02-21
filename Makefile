PROJECT = cttap

include erlang.mk

example_cttap_test:
	mkdir -p logs/example_cttap
	make ct-cttap_usage CT_OPTS='-ct_hooks cttap "[{filename, \"test.tap\"}]" -logdir logs/example_cttap'

cttap_tests: example_cttap_test
	make ct-cttap
