PROJECT = ctap

include erlang.mk

example_ctap_test:
	mkdir -p logs/example_ctap
	make ct-ctap_usage CT_OPTS="-ct_hooks ctap -logdir logs/example_ctap"
