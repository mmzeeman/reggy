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
       ?_test(normal_exit_unregisters_t()),
       ?_test(multi_reg_t())
     ]
    }.

multi_reg_t() ->
    Reg = multi_reg_t,
    reggy:start(Reg),

    %% Register a process under multiple names.
    ?assertEqual(undefined, reggy:whereis_name({Reg, 1})),
    ?assertEqual(yes, reggy:register_name({Reg, 1}, self())),
    ?assertEqual(self(), reggy:whereis_name({Reg, 1})),
    ?assertEqual(no, reggy:register_name({Reg, 1}, self())),
    ?assertEqual(self(), reggy:whereis_name({Reg, 1})),
    ?assertEqual(yes, reggy:register_name({Reg, <<"myname">>}, self())),
    ?assertEqual(self(), reggy:whereis_name({Reg, 1})),
    ?assertEqual(self(), reggy:whereis_name({Reg, <<"myname">>})),
    ok.

exit_unregisters_t() ->
    Reg = exit_unregisters_t,
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
    Reg = normal_exit_unregisters_t,
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
