-module(cttap_usage_SUITE).

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
-export([
         passing_test/1,
         failing_test/1,
         test_description/1,
         todo_test/1,
         skip_test/1,
         diagnostic_test/1,
         passing_test_in_group/1,
         failing_test_in_group/1,
         test_description_in_group/1,
         todo_test_in_group/1,
         skip_test_in_group/1,
         group_order_1/1,
         group_order_2/1,
         group_order_3/1
         ]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [
     passing_test,
     failing_test,
     test_description,
     todo_test,
     skip_test,
     diagnostic_test,
     {group, passing},
     {group, failing},
     {group, description},
     {group, todo},
     {group, skip},
     {group, order}
    ].

suite() ->
    [{timetrap, {seconds, 30}}].

groups() ->
    [{passing, [], [passing_test_in_group]},
     {failing, [], [failing_test_in_group]},
     {description, [], [test_description_in_group]},
     {todo, [], [todo_test_in_group]},
     {skip, [], [skip_test_in_group]},
     {order, [], [
                group_order_1,
                group_order_2,
                group_order_3
                ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

group(_GroupName) ->
    [].

init_per_group(skip, _Config) ->
    {skip, "it's part of the test"};
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

passing_test(_Config) ->
    % Passing test
    passed.

failing_test(_Config) ->
    % Failing test (badmatch)
    1 = 2.

test_description(_Config) ->
    % Description should just be the test function name
    ok.

todo_test(_Config) ->
    % todo test
    {skip, todo}.

skip_test(_Config) ->
    % Skip this test
    {skip, "I'm lazy"}.

diagnostic_test(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.

passing_test_in_group(_Config) ->
    % Passing test in group
    passed.

failing_test_in_group(_Config) ->
    % Failing test in group (badmatch)
    1 = 2.

test_description_in_group(_Config) ->
    % Description should just be the test function name and group name
    ok.

todo_test_in_group(_Config) ->
    % todo test in todo group
    {skip, todo}.

skip_test_in_group(_Config) ->
    % This test should be skipped since it's in the skip group
    this_test_should_be_skipped.

group_order_1(_Config) ->
    % Passing test
    passed.

group_order_2(_Config) ->
    % Failing test (badmatch)
    1 = 2.

group_order_3(_Config) ->
    % Passing test
    ok.
