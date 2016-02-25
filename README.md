#CTTAP
*Common Test Test Anything Protocol*

A TAP producer that integrates with existing Common Test suites via a Common Test hook. CTTAP provides a simple way to generate TAP output without having to modify existing test code.

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

##Similar Projects

* An complete testing library with TAP output: [https://github.com/ngerakines/etap](https://github.com/ngerakines/etap)
* A TAP producer for Elixir's ExUnit: [https://github.com/Stratus3D/Spout](https://github.com/Stratus3D/Spout)

##Known Issues
No known issues.

##Contributing
Feel free to create an issue or pull request on GitHub ([https://github.com/Stratus3D/cttap/issues](https://github.com/Stratus3D/cttap/issues)) if you find a bug or see something that could be improved.
