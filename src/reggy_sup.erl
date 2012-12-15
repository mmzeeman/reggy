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

%%% The top-level supervisor of the registration server.

-module(reggy_sup).
-behaviour(supervisor).

-export([init/1]).

-export([start_link/0, start_reg/1, stop_reg/1]).


% @doc
start_link() ->
    supervisor:start_link({local, reggy_sup}, ?MODULE, []).

% @doc
init([]) ->
    {ok, {{simple_one_for_one, 30, 3600}, 
    	  [{reggy_reg, {reggy_reg, start_link, []},
            permanent, 5000, worker, [reggy_reg]}]}}.

% @doc Start a process registry
start_reg(Name) ->
    case supervisor:start_child(reggy_sup, [Name]) of
    	{error, {already_started, Pid}} ->
    		{ok, Pid};
    	{ok, Pid} -> 
    		{ok, Pid}
    end.

% @doc Stop 
stop_reg(_Name) ->
    ok.
