-module(ctap_usage_SUITE).

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
         skip_test_in_group/1
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
     passing_test_in_group,
     failing_test_in_group,
     test_description_in_group,
     todo_test_in_group,
     skip_test_in_group
    ].

suite() ->
    [{timetrap, {seconds, 30}}].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

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

passing_test(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.

failing_test(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.

test_description(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.

todo_test(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.

skip_test(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.

diagnostic_test(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.

passing_test_in_group(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.

failing_test_in_group(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.

test_description_in_group(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.

todo_test_in_group(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.

skip_test_in_group(_Config) ->
    % TODO: Complete test
    % Remember to remove `ok` when complete
    ok.
