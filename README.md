#CTTAP
*Common Test Test Anything Protocol*

A TAP producer that integrates with Common Test via a Common Test Hook.

##Installation
Add CTTAP as a test dependency in your project:

erlang.mk:

    TEST_DEPS = cttap
    dep_cttap = git https://github.com/Stratus3D/cttap.git master

Rebar and Rebar3:

    {deps, [app_name,
        {rebar, ".*",
         {git, "git://github.com/Stratus3D/cttap.git", {branch, "master"}}}
    }

##Usage

Add cttap as a ct_hook:

erlang.mk:

    CT_OPTS ='-ct_hooks cttap "[{filename, \"../test.tap\"}]"'

Rebar:

    {ct_extra_params, "-ct_hooks cttap '[{filename, \"../test.tap\"}]'"}

Rebar3:

    {ct_opts, [cttap]}
