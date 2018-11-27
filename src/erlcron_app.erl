-module(erlcron_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    init_datetime(),
    erlcron_sup:start_link().

stop(_State) ->
    ok.


init_datetime() ->
    mochiglobal:put(delta_seconds, 0),
    ok.

