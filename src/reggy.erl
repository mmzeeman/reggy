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

-module(reggy).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([start/1]).

% callback functions for registering with {via, reggy, {RegistryName, Name}}
-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).


%% @doc Start a registry. It will be available under Name. 
%%
-spec start(Name) -> {ok, Pid} when
      Name :: atom() | {local, atom()} | {global, atom()} | {via, module(), atom()},
      Pid :: pid().
start(Name) ->
	reggy_sup:start_reg(Name).



%% @doc Register Name on Registry. Returns 'yes' iff the process was registered.
%%
-spec register_name({Registry, Name}, Pid) -> 'yes' | 'no' when
	  Registry :: atom(),
      Name :: term(),
      Pid :: pid().
register_name({Registry, Name}, Pid) ->
	case reggy_reg:register(Registry, Name, Pid) of
		ok -> yes;
		_ -> no
	end.


%% @doc Unregister the process with Name.
%%
-spec unregister_name({Registry, Name}) -> _ when
      Registry :: atom(),
      Name :: term().
unregister_name({Registry, Name}) ->
	reggy_reg:unregister(Registry, Name).


%% @doc Lookup the pid of Name on Registry.
%%
-spec whereis_name({Registry, Name}) -> pid() | 'undefined' when
      Registry :: atom(),
      Name :: term().
whereis_name({RegistryName, Name}) ->
	reggy_reg:whereis(RegistryName, Name).


%% @doc Send Msg to Name available on Registry.
%%
-spec send({Registry, Name}, Msg) -> Pid when
      Registry :: atom(),
      Name :: term(),
      Msg :: term(),
      Pid :: pid().
send({Registry, Name}=N, Msg) ->
	case reggy_reg:whereis(Registry, Name) of
		undefined ->
	 		exit({badarg, {N, Msg}});
	 	Pid when is_pid(Pid) ->
	 		Pid ! Msg,
	 		Pid
	end.
	