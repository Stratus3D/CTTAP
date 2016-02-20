#CTAP
*Common Test Test Anything Protocol*

##Installation
Add as a test dep.

##Usage

Add ctap as a ct_hook:

erlang.mk:

    CT_OPTS = -ct_hooks ctap

Rebar:

    {ct_extra_params, "-ct_hooks ctap"}

Rebar3:

    {ct_opts, [ctap]}
