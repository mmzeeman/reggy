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

-module(reggy_app).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

% @doc Start reggy supervisor.
start(_StartType, _StartArgs) ->
    reggy_sup:start_link().

% @doc and stop it.
stop(_State) ->
    ok.