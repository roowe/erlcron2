-module(erlcron_tests).

-include_lib("eunit/include/eunit.hrl").
-define(MFA, {io, fwrite, [" It's running~n"]}).

%%%===================================================================
%%% Types
%%%===================================================================
cron_test_() ->
    {setup,
     fun() ->
             application:start(erlcron),
             application:set_env(qdate, default_timezone, "Asia/Chongqing")
     end,
     fun(_) ->
             application:stop(erlcron)
     end,
     {with, [
             fun monthly_cron_test/1,
             fun weekly_cron_test/1
            ]}}.

%% Time jumps ahead one day so we should see the alarms from both days.
monthly_cron_test(_) ->    
    receive_clean(),
    erlcron:set_datetime({{2014,9,15}, {8,0,0}}),

    Self = self(),
    %% 每月2日，0:2:0执行
    erlcron:cron({monthly_cron, "0 2 0 2 * *", {erlcron_util, send, [Self, monthly_cron_test_ack]}}),
    erlcron:set_datetime({{2014, 11, 2}, {0, 1, 58}}),
    ?assertMatch(ok, receive
                         monthly_cron_test_ack -> ok
                     after
                        10 -> timeout
                     end),

    ?assertMatch(ok, receive
                         monthly_cron_test_ack -> ok
                     after
                         2000 -> timeout
                     end).

weekly_cron_test(_) ->
%% September 2014
%% Su Mo Tu We Th Fr Sa
%%     1  2  3  4  5  6
%%  7  8  9 10 11 12 13
%% 14 15 16 17 18 19 20
%% 21 22 23 24 25 26 27
%% 28 29 30
    receive_clean(),
    erlcron:set_datetime({{2014,9,17}, {0, 0, 59}}),
    Self = self(),
    erlcron:cron({weekly_cron, "0 1,10 0,10 * * 3,7", {erlcron_util, send, [Self, weekly_cron_test_ack]}}),
    ?assertMatch(ok, receive
                         weekly_cron_test_ack-> ok
                     after
                         1500 -> timeout
                     end),
    erlcron:set_datetime({{2014,9,21}, {9, 9, 58}}),
    ?assertMatch(ok, receive
                         weekly_cron_test_ack -> ok
                     after
                         2500 -> timeout
                     end).


receive_clean() ->
    receive
        _ ->
            receive_clean()
    after 
        0 ->
            ok
    end.
