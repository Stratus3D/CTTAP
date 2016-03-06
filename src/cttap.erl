%%--------------------------------------------------------------------
%% @doc
%% Test Anything Protocol Common Test Hook integration module.
%%
%% @end
%%--------------------------------------------------------------------
-module(cttap).

%% Common Test Hook callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_init_per_group/4]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3]).

-export([terminate/1]).

-record(state, { file_handle, total, suite_total, ts, tg, tcs, tgtcs, data }).

-define(default_tap_file, "tap_output").

%%%===================================================================
%%% Common Test Hook callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Return a unique id for this CTH.
%%
%% @end
%%--------------------------------------------------------------------
id(Opts) ->
  case proplists:get_value(filename, Opts) of
      undefined -> ?default_tap_file;
      Path -> filename:absname(Path)
  end.

%%--------------------------------------------------------------------
%% @doc
%% Always called before any other callback function. Use this to initiate
%% any common state.
%%
%% @end
%%--------------------------------------------------------------------
init(Id, _Opts) ->
    {ok,TapFile} = file:open(Id,[write]),
    {ok, #state{file_handle = TapFile, total = 0, data = []}}.

%%--------------------------------------------------------------------
%% @doc
%% Called before init_per_suite is called.
%%
%% @end
%%--------------------------------------------------------------------
pre_init_per_suite(_Suite,Config,State) ->
    {Config, State#state{ suite_total = 0, tcs = [] }}.

%%--------------------------------------------------------------------
%% @doc
%% Called after init_per_suite.
%%
%% @end
%%--------------------------------------------------------------------
post_init_per_suite(Suite,_Config,Return,State) ->
    Data = {suites, Suite, skipped, 0, []},
    {Return, State#state{ data = [Data|State#state.data]}}.

%%--------------------------------------------------------------------
%% @doc
%% Called before end_per_suite.
%%
%% @end
%%--------------------------------------------------------------------
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%%--------------------------------------------------------------------
%% @doc
%% Called after end_per_suite.
%%
%% @end
%%--------------------------------------------------------------------
post_end_per_suite(Suite,_Config,Return,State) ->
    [_|NewData] = State#state.data,
    Data = {suites, Suite, ran, State#state.suite_total, lists:reverse(State#state.tcs)},
    {Return, State#state{ data = [Data | NewData] ,
                          total = State#state.total + State#state.suite_total } }.

%%--------------------------------------------------------------------
%% @doc
%% Called before each init_per_group.
%%
%% @end
%%--------------------------------------------------------------------
pre_init_per_group(_Group,Config,State) ->
    {Config, State}.

%%--------------------------------------------------------------------
%% @doc
%% Called after each init_per_group.
%%
%% @end
%%--------------------------------------------------------------------
post_init_per_group(Group,_Config,Return,State) ->
    Data = {group, Group, Return, [], incomplete},
    {Return, State#state{tg=Data, tgtcs=[]}}.

%%--------------------------------------------------------------------
%% @doc
%% Called after each end_per_group.
%%
%% @end
%%--------------------------------------------------------------------
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%%--------------------------------------------------------------------
%% @doc
%% Called after each end_per_group.
%%
%% @end
%%--------------------------------------------------------------------
post_end_per_group(_Group,_Config,Return,State) ->
    {group, GroupName, _, _, _}=State#state.tg,
    OtherTests=State#state.tcs,
    NewGroupData={group, GroupName, Return, lists:reverse(State#state.tgtcs)},
    {Return, State#state{tg=undefined, tcs=[NewGroupData|OtherTests], tgtcs=undefined}}.

%%--------------------------------------------------------------------
%% @doc
%% Called before each test case.
%%
%% @end
%%--------------------------------------------------------------------
pre_init_per_testcase(_TC,Config,State) ->
    {Config, State#state{ ts = timestamp(), suite_total = State#state.suite_total + 1 } }.

%%--------------------------------------------------------------------
%% @doc
%% Called after each test case.
%%
%% @end
%%--------------------------------------------------------------------
post_end_per_testcase(TC,_Config,Return,State) ->
    TCInfo = {testcase, TC, Return, timer:now_diff(timestamp(), State#state.ts)},
    NewState = case State#state.tg of
        {group, _, _, _, incomplete} ->
            State#state{ ts = undefined, tgtcs = [TCInfo | State#state.tgtcs] };
        _ ->
            State#state{ ts = undefined, tcs = [TCInfo | State#state.tcs] }
    end,

    {Return, NewState}.

%%--------------------------------------------------------------------
%% @doc
%% Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
%%
%% @end
%%--------------------------------------------------------------------
on_tc_fail(_TC, _Reason, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Called when a test case is skipped by either user action
%% or due to an init function failing.
%%
%% @end
%%--------------------------------------------------------------------
on_tc_skip(_TC, _Reason, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Called when the scope of the CTH is done
%%
%% @end
%%--------------------------------------------------------------------
terminate(State) ->
    TapOutput = tapify(State#state.data, State#state.total),
    lists:foreach(fun(Line) ->
                          io:format(State#state.file_handle, "~s~n", [Line])
                  end, TapOutput),
    file:close(State#state.file_handle),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tapify(Data, Total) ->
    {Output, _Count} = process_suites(Data, 1, []),
    [version(), test_plan_line(Total) |lists:reverse(Output)].

process_suites([], Count, Output) ->
    {Output, Count};
process_suites([{suites, Suite, Status, _Num, TestCases}|Suites], Count, Output) ->
    Header = diagnostic_line(["Starting ", atom_to_list(Suite)]),
    Footer = case Status of
                 skipped ->
                     diagnostic_line(["Skipped ", atom_to_list(Suite)]);
                 ran ->
                     diagnostic_line(["Completed ", atom_to_list(Suite)])
             end,
    {TestcaseOutput, NewCount} = process_testcases(TestCases, Count, [Header|Output]),
    process_suites(Suites, NewCount, [Footer|TestcaseOutput]).

process_testcases([], Count, Output) ->
    {Output, Count};
process_testcases([{group, Name, Return, GroupTestCases}|TestCases], Count, Output) ->
    Header = diagnostic_line(["Starting ", atom_to_list(Name), " group"]),
    Footer = diagnostic_line(["Completed ", atom_to_list(Name), " group. return value: ", io_lib:format("~w", [Return])]),
    {NewOutput, NewCount} = process_testcases(GroupTestCases, Count, [Header|Output]),
    process_testcases(TestCases, NewCount, [Footer|NewOutput]);
process_testcases([{testcase, Name, Return, _Num}|TestCases], Count, Output) ->
    % TODO: Figure out how to access the IO log here and log IO as diagnostic output
    Line = case Return of
        {skip, todo} ->
            test_todo(Count, Name, undefined);
        {skip, Reason} ->
            test_skip(Count, Name, Reason);
        {error, Reason} ->
            test_fail(Count, [atom_to_list(Name), " reason: ", io_lib:format("~w", [Reason])]);
        Value ->
            test_success(Count, [atom_to_list(Name), " return value: ", io_lib:format("~w", [Value])])
    end,
    process_testcases(TestCases, Count + 1, [Line|Output]).

timestamp() ->
    os:timestamp().

%%%===================================================================
%%% Test Anything Protocol functions
%%%===================================================================

version() ->
    <<"TAP version 13">>.

test_plan_line(NumTests) ->
    io_lib:format("1..~B", [NumTests]).

% I can't think of a reason all the test suites would need to abort
%bail_out(Reason) ->
%    io_lib("Bail out! ~s", [Reason]).

% I can't think of a reason all the test suites would need to be skipped
%test_plan_line_skip(NumTests, Reason) ->
%    io_lib:format("1..~B ~s", [NumTests, Reason]).

test_success(Number, Description) ->
    io_lib:format("ok ~B ~s", [Number, Description]).

test_fail(Number, Description) ->
    io_lib:format("not ok ~B ~s", [Number, Description]).

test_skip(Number, Description, Reason) ->
    io_lib:format("ok ~B ~s # SKIP ~s", [Number, Description, Reason]).

test_todo(Number, Description, Reason) ->
    case Reason of
        undefined ->
            io_lib:format("not ok ~B ~s # TODO", [Number, Description]);
        _ ->
            io_lib:format("not ok ~B ~s # TODO ~s", [Number, Description, Reason])
    end.

diagnostic_line(Message) ->
    ["# "|Message].

% We currently don't need this function
%diagnostic_multiline(Message) when is_list(Message) ->
%    diagnostic_multiline(list_to_binary(Message));
%diagnostic_multiline(Message) when is_binary(Message) ->
%    Lines = binary:split(Message, <<"~n">>, [global]),
%    [diagnostic_line(Line) || Line <- Lines].
