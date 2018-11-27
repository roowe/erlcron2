-module(erlcron_time).

%% -compile([export_all]).

-export([
         parse/1,
         next/2
        ]).

-include("internal.hrl").

-record(bounds, 
        {
         min = 0, max
        }).

-define(Second, 1). %%  Seconds field, default 0
-define(Minute, 2). %% Minutes field, default 0
-define(Hour, 4). %% Hours field, default 0
-define(Dom, 8). %% Day of month field, default *
-define(Month, 16). %% Month field, default *
-define(Dow, 32). %% Day of week field, default *
-define(MaxUint64, 18446744073709551615). %% 1<<64 - 1


-define(SecondBounds, #bounds{max=59}).
-define(MinuteBounds, ?SecondBounds).
-define(HourBounds, #bounds{max=23}).
-define(DomBounds, #bounds{min=1,max=31}).
-define(MonthBounds, #bounds{min = 1, max = 12}).
-define(DowBounds, #bounds{min = 1, max = 7}).


parse([$@|_]=Field) ->
    parse_descriptor(Field);
parse(SS) ->
    case split(SS, " ") of
        [_, _, _, _, _, _] = S ->
            SBounds = lists:zip(S, 
                                [?SecondBounds, ?MinuteBounds, ?HourBounds,
                                 ?DomBounds, ?MonthBounds, ?DowBounds]),
            case lists:foldl(fun(_, {error, Err}) ->
                                     {error, Err};
                                ({Field, Bounds}, Acc) ->
                                     case get_field(Field, Bounds) of
                                         {ok, Bits} ->
                                             [Bits|Acc];
                                         Err ->
                                             Err
                                     end
                             end, [], SBounds) of
                {error, Err} ->
                    {error, Err};
                [Dow, Month, Dom, Hour, Minute, Second] ->
                    {ok, #spec_schedule{
                            second = Second,
                            minute = Minute, 
                            hour = Hour, 
                            dom = Dom,
                            month = Month,
                            dow = Dow
                           }}
            end;
        _ ->
            {error, "error spec string"}
    end.


get_field(Field, Bounds) ->
    Ranges = split(Field, ","),
    lists:foldl(fun (_, {error, Error}) ->
                        {error, Error};
                    (Range, {ok, Bits}) ->
                        case get_range(Range, Bounds) of
                            {ok, Bit} ->
                                {ok, Bits bor Bit};
                            {error, Error} ->
                                {error, Error}
                        end
                end, {ok, 0}, Ranges).
%% *
%% number == number-number
%% start-end
%% start-end/step
get_range(Expr, #bounds{
                   min = Min,
                   max = Max
                  }) ->
    [Range | Steps] = split(Expr, "/"),
    [Low | Highs] = split(Range, "-"),

    if 
        Low == "*" orelse Low == "?" ->
            Start = Min,
            End = Max;
        true ->
            Start = list_to_integer(Low),
            case Highs of 
                [] ->
                    End = Start;
                [V] ->
                    End = list_to_integer(V)
            end                   
    end,
    case Steps of 
        [] ->
            Step = 1;
        [StepV] ->
            Step = list_to_integer(StepV)
    end,
    if 
        Start >= Min andalso
        End =< Max andalso 
        Start =< End andalso
        Step > 0 ->
            {ok, get_bits(Start, End, Step)};
        true ->
            {error, "get_range error parms"}
    end.

split(S, Ch) ->
    [string:strip(SubS, both, $ ) || SubS <- string:split(S, Ch, all)].

get_bits(Min, Max, 1) ->
    ( bnot (?MaxUint64 bsl (Max + 1))) band (?MaxUint64 bsl Min);
get_bits(Min, Max, Step) ->
    lists:foldl(fun(I, Bits) ->
                        Bits bor (1 bsl I)
                end, 0, lists:seq(Min, Max, Step)).

all(#bounds{
       min = Min,
       max = Max
      }) ->
    get_bits(Min, Max, 1).

parse_descriptor("@annually") ->
    parse_descriptor("@yearly");
parse_descriptor("@yearly") ->
    {ok, #spec_schedule{
            second = 1 bsl ?SecondBounds#bounds.min,
            minute = 1 bsl ?MinuteBounds#bounds.min,
            hour = 1 bsl ?HourBounds#bounds.min,
            dom = 1 bsl ?DomBounds#bounds.min,
            month = 1 bsl ?MonthBounds#bounds.min,
            dow = all(?DowBounds)
           }};
parse_descriptor("@monthly") ->
    {ok, #spec_schedule{
            second = 1 bsl ?SecondBounds#bounds.min,
            minute = 1 bsl ?MinuteBounds#bounds.min,
            hour = 1 bsl ?HourBounds#bounds.min,
            dom = 1 bsl ?DomBounds#bounds.min,
            month = all(?MonthBounds),
            dow = all(?DowBounds)
           }};
parse_descriptor("@weekly") ->
    {ok, #spec_schedule{
            second = 1 bsl ?SecondBounds#bounds.min,
            minute = 1 bsl ?MinuteBounds#bounds.min,
            hour = 1 bsl ?HourBounds#bounds.min,
            dom = all(?DomBounds),
            month = all(?MonthBounds),
            dow = 1 bsl ?DowBounds#bounds.min
           }};
parse_descriptor("@midnight") ->
    parse_descriptor("@daily");
parse_descriptor("@daily") ->
    {ok, #spec_schedule{
            second = 1 bsl ?SecondBounds#bounds.min,
            minute = 1 bsl ?MinuteBounds#bounds.min,
            hour = 1 bsl ?HourBounds#bounds.min,
            dom = all(?DomBounds),
            month = all(?MonthBounds),
            dow = all(?DowBounds)
           }};
parse_descriptor("@hourly") ->
    {ok, #spec_schedule{
            second = 1 bsl ?SecondBounds#bounds.min,
            minute = 1 bsl ?MinuteBounds#bounds.min,
            hour = all(?HourBounds),
            dom = all(?DomBounds),
            month = all(?MonthBounds),
            dow = all(?DowBounds)
           }};
parse_descriptor("@every " ++ S) ->
    V = list_to_integer(S),
    {ok, #constant_delay_schedule{
            delay = V
           }}.

next(#constant_delay_schedule{
        delay = Delay
       }, Timestamp) ->
    qdate:add_unit(seconds, Delay, Timestamp);
next(#spec_schedule{}=Spec, Timestamp) ->
    next(Spec, Timestamp, false).

next(Spec, Timestamp, Added) ->
    next_month(Spec, Timestamp, Added).

next_month(Spec, Timestamp, Added) ->
    {{Y, M, _}, _} = qdate:to_date(Timestamp),
    if 
        (1 bsl M) band Spec#spec_schedule.month > 0 ->
            next_day(Spec, Timestamp, Added);
        true ->
            NewTimestamp = if 
                               Added == false ->
                                   {{Y, M, 1}, {0,0,0}};
                               true ->
                                   Timestamp
                           end,
            Timestamp1 = qdate:add_unit(months, 1, NewTimestamp),
            case qdate:to_date(Timestamp1) of
                {{_, 1, _}, _} ->
                    next(Spec, Timestamp1, true);
                _ ->
                    next_month(Spec, Timestamp1, true)
            end
    end.

next_day(Spec, Timestamp, Added) ->
    {{Y, M, D}, _} = qdate:to_date(Timestamp),
    DW = calendar:day_of_the_week({Y, M, D}),
    if 
        (1 bsl D) band Spec#spec_schedule.dom > 0 andalso
        (1 bsl DW) band Spec#spec_schedule.dow > 0 ->
            next_hour(Spec, Timestamp, Added);
        true ->
            NewTimestamp = if 
                               Added == false ->
                                   {{Y, M, D}, {0,0,0}};
                               true ->
                                   Timestamp
                           end,
            Timestamp1 = qdate:add_unit(days, 1, NewTimestamp),
            case qdate:to_date(Timestamp1) of
                {{_, _, 1}, _} ->
                    next(Spec, Timestamp1, true);
                _ ->
                    next_day(Spec, Timestamp1, true)
            end
    end.

next_hour(Spec, Timestamp, Added) ->
    %% io:format("hour ~p~n", [qdate:to_date(Timestamp)]),
    {Date, {H, _, _}} = qdate:to_date(Timestamp),
    if 
        (1 bsl H) band Spec#spec_schedule.hour > 0 ->
            next_minute(Spec, Timestamp, Added);
        true ->
            NewTimestamp = if 
                               Added == false ->
                                   {Date, {H,0,0}};
                               true ->
                                   Timestamp
                           end,
            Timestamp1 = qdate:add_unit(hours, 1, NewTimestamp),
            case qdate:to_date(Timestamp1) of
                {_, {0, _, _}} ->
                    next(Spec, Timestamp1, true);
                _ ->
                    next_hour(Spec, Timestamp1, true)
            end
    end.           

next_minute(Spec, Timestamp, Added) ->
    {Date, {H, M, _}} = qdate:to_date(Timestamp),
    %% ?PRINT("next minute ~p ~p~n", [qdate:to_date(Timestamp), Added]),
    if 
        (1 bsl M) band Spec#spec_schedule.minute > 0 ->
            next_second(Spec, Timestamp, Added);
        true ->
            NewTimestamp = if 
                               Added == false ->
                                   {Date, {H,M,0}};
                               true ->
                                   Timestamp
                           end,
            Timestamp1 = qdate:add_unit(minutes, 1, NewTimestamp),
            case qdate:to_date(Timestamp1) of
                {_, {_, 0, _}} ->
                    next(Spec, Timestamp1, true);
                _ ->
                    next_minute(Spec, Timestamp1, true)
            end
    end.    

next_second(Spec, Timestamp, Added) ->
    {Date, {H, M, S}} = qdate:to_date(Timestamp),
    %% ?PRINT("next second ~p ~p~n", [qdate:to_date(Timestamp), Added]),
    if 
        (1 bsl S) band Spec#spec_schedule.second > 0 ->
            Timestamp;
        true ->
            NewTimestamp = if 
                               Added == false ->
                                   {Date, {H,M,S}};
                               true ->
                                   Timestamp
                           end,
            Timestamp1 = qdate:add_unit(seconds, 1, NewTimestamp),
            case qdate:to_date(Timestamp1) of
                {_, {_, _, 0}} ->
                    next(Spec, Timestamp1, true);
                _ ->
                    next_second(Spec, Timestamp1, true)
            end
    end.  

%% test() ->
%%     qdate:set_timezone("Asia/Chongqing"),
%%     {ok, Spec} = parse(["4-50", "26", "1-17/3", "*", "4", "*"]),
%%     qdate:to_date(next(Spec, os:timestamp())).
        
