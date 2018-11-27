%%% @copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the BSD License; you may not use
%%% this file except in compliance with the License.
-define(ONE_DAY, (24 * 60 * 60)).
-define(PRINT(Format, Args), 
    io:format("(~p:~p:~p) : " ++ Format, 
              [self(), ?MODULE, ?LINE] ++ Args)).
-define(ERROR, error_logger:error_msg).

-record(spec_schedule, 
        {
         second = 0,
         minute = 0, 
         hour = 0, 
         dom = 0,
         month = 0,
         dow = 0
        }).

-record(constant_delay_schedule,
        {
         delay
        }).

%%-define(PRINT(Format, Args), ok).
