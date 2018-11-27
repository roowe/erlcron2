-module(erlcron).

-include("internal.hrl").

-export([test/0,
         cron/1,         
         cancel/1,
         datetime/0,
         set_datetime/1]).

cron(Job) ->
    erlcron_agent_sup:add_job(Job).

cancel(Name) ->
    cron_agent:cancel(Name).

datetime() ->
    calendar:gregorian_seconds_to_datetime(mochiglobal:get(reference_gregorian_seconds)).

set_datetime(DateTime) ->
    %% ?ERROR("set_datetime ~p~n", [DateTime]),
    Delta = qdate:to_unixtime(DateTime) - qdate:unixtime(),
    mochiglobal:put(delta_seconds, Delta),
    [erlcron_agent:set_datetime(Pid, Delta) || Pid <- erlcron_agent_sup:all_child_pids()].


test() ->
    application:start(erlcron),
    qdate:set_timezone("Asia/Chongqing"),
    erlcron:set_datetime({{2014,9,15}, {8,0,0}}),
    erlcron:cron({second, "* * * * * *", {io, format, [second]}}),
    erlcron:cron({minute, "0 * * * * *", {io, format, [minute]}}).

