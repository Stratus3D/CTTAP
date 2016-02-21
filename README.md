#CTTAP
*Common Test Test Anything Protocol*

A TAP producer that integrates with Common Test via a Common Test Hook.

##Installation
Add CTTAP as a test dependency in your project:

erlang.mk:

    TEST_DEPS = cttap
    dep_cttap = git repo master

Rebar:

    TODO: Add code for rebar dep

##Usage

Add cttap as a ct_hook:

erlang.mk:

    CT_OPTS = -ct_hooks cttap

Rebar:

    {ct_extra_params, "-ct_hooks cttap"}

Rebar3:

    {ct_opts, [cttap]}
