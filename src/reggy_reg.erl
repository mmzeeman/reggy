%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman
%%
%% @doc Flexible Erlang Process Registries
%%
%% Copyright 2012 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(reggy_reg).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(gen_server).

% api exports
-export([
    start_link/1, 
    register/3, 
    unregister/2, 
    whereis/2, 
    names/2
]).

% behaviour exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {table, pids}).

% @doc
%
start_link({local, Name}=N) ->
    gen_server:start_link(N, ?MODULE, Name, []);
start_link({global, Name}=N) ->
    gen_server:start_link(N, ?MODULE, Name, []);
start_link(Name) ->
    start_link({local, Name}).
    

% @doc Register pid as Name.
%
register(RegistryName, Name, Pid) ->
    gen_server:call(RegistryName, {register, Name, Pid}).
    
% @doc Unregister Name
%
unregister(RegistryName, Name) ->
    gen_server:call(RegistryName, {unregister, Name}).

% @doc Get the pid of the process with Name.
%
whereis(RegistryName, Name) ->
    case ets:lookup(table_name(RegistryName), Name) of
        [{Name, _Ref, Pid}] -> 
            Pid;
        _ -> 
            undefined
    end.
    
% @doc 
names(Pid, RegistryName) ->
    gen_server:call(RegistryName, {names, Pid}).

%
init(Name) ->
    Table = ets:new(table_name(Name), [named_table, set, protected, 
        {read_concurrency, true}]),
    {ok, #state{table=Table, pids=dict:new()}}.

%%
handle_call({register, Name, Pid}, _From, #state{table=Table}=State) ->
    case ets:lookup(Table, Name) of
        [{Name, _Ref, _Pid}] ->
            {reply, {error, already_registered}, State};
        _ ->
            {reply, ok, do_register(Name, Pid, State)}
    end;
handle_call({unregister, Name}, _From, #state{table=Table}=State) ->
    case ets:lookup(Table, Name) of
        [{Name, Ref, Pid}] ->
            {reply, ok, do_unregister(Name, Ref, Pid, State)};
        _ ->
            {reply, ok, State}
    end;
handle_call({names, Pid}, _From, #state{pids=Pids}=State) ->
    case dict:find(Pid, Pids) of
        error ->
            {reply, [], State};
        {ok, {_Ref, Names}} ->
            {reply, Names, State}
    end;
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

%%
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

%% A registered process stopped. Unregister the process.
%%
handle_info({'DOWN', Ref, _Process, Pid, _Reason}, #state{table=Table, pids=Pids}=State) ->
    case dict:find(Pid, Pids) of
        error ->
            {noreply, State};
        {ok, {Ref, ItsNames}} ->
            Pids1 = dict:erase(Pid, Pids),
            Ms = [{{Name, '_', '_'}, [], [true]} || Name <- ItsNames],
            N = length(ItsNames),
            N = ets:select_delete(Table, Ms),
            {noreply, State#state{pids=Pids1}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{table=Table}) ->
    ets:delete(Table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal stuff
%%

%% @doc Register Pid under the specified name. The process will be monitored and 
%% unregistered when it exits. 
%% 
do_register(Name, Pid, #state{table=Table, pids=Pids}=State) ->
    %% Register in the pids dict.
    {TheRef, UpdatedPids} = case dict:find(Pid, Pids) of
                            {ok, {Ref, _}} ->
                                {Ref, dict:update(Pid, fun({R, TheNames}) -> 
                                    {R, [Name | TheNames]} end, Pids)};
                            error ->
                                Ref = erlang:monitor(process, Pid),
                                {Ref, dict:store(Pid, {Ref, [Name]}, Pids)}
                         end,
    ets:insert(Table, {Name, TheRef, Pid}),
    State#state{pids=UpdatedPids}.

%% @doc Unregister the Name. When process no longer is known under any other name, 
%% it is demonitored and removed.
%%
do_unregister(Name, Ref, Pid, #state{table=Table, pids=Pids}=State) ->
    {ok, {Ref, ItsNames}} = dict:find(Pid, Pids),
    UpdatedPids = case lists:delete(Name, ItsNames) of
                       [] ->
                           erlang:demonitor(Ref, [flush]),
                           dict:erase(Pid, Pids);
                       ItsNames1 ->
                           dict:store(Pid, {Ref, ItsNames1}, Pids)
                  end,
    ets:delete(Table, Name),
    State#state{pids=UpdatedPids}.


%% @doc
%%
table_name(RegistryName) ->
    erlang:list_to_atom(erlang:atom_to_list(RegistryName) ++ "_reg").
