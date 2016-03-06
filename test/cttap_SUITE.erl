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
    [UsageSuiteHeader, PassingOk, Failing, PassingDescription, Todo, Skip, Diagnostic|Groups] = UsageSuite,
    passing_test(PassingOk, 4, passing_test, ok),
    failing_test(Failing, 5, failing_test),
    passing_test(PassingDescription, 6, test_description, ok),
    todo_test(Todo, 7, todo_test),
    skipped_test(Skip, 8, skip_test, <<"I'm lazy">>),
    passing_test(Diagnostic, 9, diagnostic_test, ok),

    % First group
    [PassingGroupHeader, PassingGroupTest, PassingGroupFooter|FailingGroup] = Groups,
    <<"# Starting passing group">> = PassingGroupHeader,
    passing_test(PassingGroupTest, 10, passing_test_in_group, ok),
    <<"# Completed passing group. return value: ok">> = PassingGroupFooter,

    % Failing group
    [FailingGroupHeader, FailingGroupTest, FailingGroupFooter|DescriptionGroup] = FailingGroup,
    <<"# Starting failing group">> = FailingGroupHeader,
    failing_test(FailingGroupTest, 11, failing_test_in_group, <<"{{badmatch,2},[{cttap_usage_SUITE,failing_test_in_group,1">>),
    <<"# Completed failing group. return value: ok">> = FailingGroupFooter,

    % Description group
    [DescriptionGroupHeader, DescriptionGroupTest, DescriptionGroupFooter|TodoGroup] = DescriptionGroup,
    <<"# Starting description group">> = DescriptionGroupHeader,
    passing_test(DescriptionGroupTest, 12, test_description_in_group, ok),
    <<"# Completed description group. return value: ok">> = DescriptionGroupFooter,

    % Todo group
    [TodoGroupHeader, TodoGroupTest, TodoGroupFooter|OrderGroup] = TodoGroup,
    <<"# Starting todo group">> = TodoGroupHeader,
    todo_test(TodoGroupTest, 13, todo_test_in_group),
    <<"# Completed todo group. return value: ok">> = TodoGroupFooter,

    % Order group
    [OrderGroupHeader, OrderGroupTest1, OrderGroupTest2, OrderGroupTest3, OrderGroupFooter, UsageSuiteFooter, _] = OrderGroup,
    <<"# Starting order group">> = OrderGroupHeader,
    passing_test(OrderGroupTest1, 14, group_order_1, ok),
    failing_test(OrderGroupTest2, 15, group_order_2, <<"{{badmatch,2},[{cttap_usage_SUITE,group_order_2,1,[{">>),
    passing_test(OrderGroupTest3, 16, group_order_3, ok),
    <<"# Completed order group. return value: ok">> = OrderGroupFooter,

    % Header and footer include the suite name
    <<"# Starting cttap_usage_SUITE">> = UsageSuiteHeader,
    <<"# Completed cttap_usage_SUITE">> = UsageSuiteFooter,
    ok.

% Private functions
passing_test(Line, Number, Test, Return) ->
    TestName = atom_to_binary(Test, utf8),
    NumberBin = integer_to_binary(Number),
    ReturnBin = list_to_binary(io_lib:format("~w", [Return])),
    Expected = <<"ok ", NumberBin/binary, " ", TestName/binary, " return value: ", ReturnBin/binary>>,
    Expected = Line.

failing_test(Line, Number, Test) ->
    failing_test(Line, Number, Test, undefined).
failing_test(Line, Number, Test, Reason) ->
    TestName = atom_to_binary(Test, latin1),
    NumberBin = integer_to_binary(Number),
    Expected = <<"not ok ", NumberBin/binary, " ", TestName/binary, " reason:">>,
    case Reason of
        undefined ->
            {0, _} = binary:match(Line, Expected, []);
        _ when is_binary(Reason) ->
            ExpectedWithReason = <<Expected/binary, " ", Reason/binary>>,
            {0, _} = binary:match(Line, ExpectedWithReason, [])
    end.

skipped_test(Line, Number, Test) ->
    skipped_test(Line, Number, Test, undefined).
skipped_test(Line, Number, Test, Reason) ->
    TestName = atom_to_binary(Test, latin1),
    NumberBin = integer_to_binary(Number),
    Expected = <<"ok ", NumberBin/binary, " ", TestName/binary, " # SKIP">>,
    case Reason of
        undefined ->
            {0, _} = binary:match(Line, Expected, []);
        _ ->
            RealExpected = <<Expected/binary, " ", Reason/binary>>,
            RealExpected = Line
    end.

todo_test(Line, Number, Test) ->
    TestName = atom_to_binary(Test, latin1),
    NumberBin = integer_to_binary(Number),
    Expected = <<"not ok ", NumberBin/binary, " ", TestName/binary, " # TODO">>,
    Expected = Line.
