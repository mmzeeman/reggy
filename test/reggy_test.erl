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

reg_test_() ->
    {foreach, local, fun setup/0, fun teardown/1,
     [ ?_test(exit_unregisters_t()),
       ?_test(normal_exit_unregisters_t())
     ]
    }.

exit_unregisters_t() ->
    Reg = exit_unregisters_test_reg,
    reggy:start(Reg),
    Self = self(),
    Pid = spawn(fun() -> process(test_proc, Reg, Self) end),
    receive registered -> ok end,

    %% The process should now be registered
    ?assertEqual(Pid, reggy:whereis_name({Reg, test_proc})),

    exit(Pid, kill), %% Give it some time to unregister.
    timer:sleep(10),

    %% The process should be unregistered.
    ?assertEqual(undefined, reggy:whereis_name({Reg, test_proc})),
    ok.

normal_exit_unregisters_t() ->
    Reg = normal_exit_unregisters_test_reg,
    reggy:start(Reg),
    Self = self(),
    Pid = spawn(fun() -> process(test_proc, Reg, Self) end),
    receive registered -> ok end,

    %% The process should now be registered
    ?assertEqual(Pid, reggy:whereis_name({Reg, test_proc})),
    Pid ! {Self, make_ref(), "Stop it"},
    timer:sleep(10), % give it some time to unregister

    %% The process should be unregistered.
    ?assertEqual(undefined, reggy:whereis_name({Reg, test_proc})),

    ok.

%%
process(Name, Registry, Pid) ->
    yes = reggy:register_name({Registry, Name}, self()),
    Pid ! registered,
    receive
        {From, Ref, Msg} -> From ! {Ref, Msg}
    end.
