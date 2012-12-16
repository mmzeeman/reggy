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

-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).


% @doc Start a registry. 
start(Name) ->
	reggy_sup:start_reg(Name).

% @doc callback function
%
% For registering via... {via, reggy, {RegistryName, Name}}
register_name({RegistryName, Name}, Pid) ->
	case reggy_reg:register(RegistryName, Name, Pid) of
		ok -> yes;
		_ -> no
	end.

unregister_name({RegistryName, Name}) ->
	reggy_reg:unregister(RegistryName, Name).

whereis_name({RegistryName, Name}) ->
	reggy_reg:whereis(RegistryName, Name).

send({RegistryName, Name}=N, Msg) ->
	case reggy_reg:whereis(RegistryName, Name) of
		undefined ->
	 		exit({badarg, {N, Msg}});
	 	Pid when is_pid(Pid) ->
	 		Pid ! Msg,
	 		Pid
	end.