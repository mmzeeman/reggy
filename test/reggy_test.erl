%% Some tests

-module(reggy_test).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    application:start(reggy).

teardown(_) ->
    application:stop(reggy).

application_start_stop_test() ->
    ?assertEqual(ok, setup()),
    ?assertEqual(ok, teardown([])).

exit_unregisters_test() ->
    setup(),
    reggy:start(test_reg),
    Self = self(),
    Pid = spawn(fun() -> process(test_proc, Self) end),
    receive registered -> ok end,

    %% The process should now be registered
    ?assertEqual(Pid, reggy:whereis_name({test_reg, test_proc})),

    exit(Pid, kill), %% Give it some time to unregister.
    timer:sleep(10),

    %% The process should be unregistered.
    ?assertEqual(undefined, reggy:whereis_name({test_reg, test_proc})),
    teardown(ok),
    ok.

normal_exit_unregisters_test() ->
    setup(),
    reggy:start(test_reg),
    Self = self(),
    Pid = spawn(fun() -> process(test_proc, Self) end),
    receive registered -> ok end,

    %% The process should now be registered
    ?assertEqual(Pid, reggy:whereis_name({test_reg, test_proc})),
    Pid ! {Self, make_ref(), "Stop it"},
    timer:sleep(10), % give it some time to unregister

    %% The process should be unregistered.
    ?assertEqual(undefined, reggy:whereis_name({test_reg, test_proc})),
    teardown(ok),
    ok.

%%
process(Name, Pid) ->
    yes = reggy:register_name({test_reg, Name}, self()),
    Pid ! registered,
    receive
        {From, Ref, Msg} -> From ! {Ref, Msg}
    end.
