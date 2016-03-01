-module(cttap_SUITE).

%% Common Test callbacks
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Test cases
-export([validate_output/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [validate_output].

suite() ->
    [{timetrap, {seconds, 30}}].

groups() ->
    [].

init_per_suite(Config) ->
    {ok,TapOutput} = file:read_file("../example_cttap/test.tap"),
    ct:pal("TapOutput: ~s", [TapOutput]),
    [{tap_output, TapOutput}|Config].

end_per_suite(_Config) ->
    ok.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

validate_output(Config) ->
    TapOutput = ?config(tap_output, Config),
    Lines = binary:split(TapOutput, <<"\n">>, [global]),
    [Version, TestPlan|Tests] = Lines,

    % First line should be the version
    <<"TAP version 13">> = Version,

    % Second line should be the test plan (skipped suite is excluded the report)
    <<"1..16">> = TestPlan,

    % Next comes the passing suite
    [SuiteHeader, Passing1, Passing2, Passing3, SuiteFooter|UsageSuite] = Tests,

    % Header and footer should include the suite name
    <<"# Starting cttap_usage_passing_SUITE">> = SuiteHeader,
    <<"# Completed cttap_usage_passing_SUITE">> = SuiteFooter,

    % Passing tests
    passing_test(Passing1, 1, passing_test_1, ok),
    passing_test(Passing2, 2, passing_test_2, ok),
    passing_test(Passing3, 3, passing_test_3, ok),

    % Then the usage suite
    [_UsageSuiteHeader, PassingOk, Failing, PassingDescription, Todo, Skip, Diagnostic|_] = UsageSuite,
    % TODO: Complete unit test
    ct:pal("Tests: ~w", [length(Tests)]),
    ok.

% Private functions
passing_test(Line, Number, Test, Return) ->
    TestName = atom_to_binary(Test, utf8),
    NumberBin = integer_to_binary(Number),
    ReturnBin = list_to_binary(io_lib:format("~w", [Return])),
    Expected = <<"ok ", NumberBin/binary, " ", TestName/binary, " Return value: ", ReturnBin/binary>>,
    Expected = Line.
