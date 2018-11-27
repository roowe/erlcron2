-module(erlcron_agent).

-behaviour(gen_server).

%% API
-export([start_link/1,
         cancel/1,
         set_datetime/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("internal.hrl").

-record(state, {
                mfa,
                spec,
                delta_seconds,
                fast_forward = false,
                timer_ref
               }).

-define(MILLISECONDS, 1000).
-define(WAIT_BEFORE_RUN, 2000).

start_link({Name, TimeConf, MFA})->
    gen_server:start_link({local, Name}, ?MODULE, [TimeConf, MFA], []).

cancel(Name) ->
    gen_server:cast(Name, shutdown).


set_datetime(PidOrName, Delta) ->
    gen_server:cast(PidOrName, {set_datetime, Delta}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([TimeConf, MFA]) -> 
    case erlcron_time:parse(TimeConf) of
        {error, Error} ->
            ?ERROR("erlcron_time:parse ~p~n", [Error]),
            {stop, normal};
        {ok, Spec} ->
            State = #state{
                       delta_seconds = mochiglobal:get(delta_seconds),
                       spec = Spec,
                       mfa = MFA
                      },
            {ok, next(State)}
    end.    

%% @private
handle_call(Msg, _From, State) ->
    ?ERROR("unknown call msg ~p", [Msg]),
    {reply, ok, State}.

%% @private
handle_cast(shutdown, State) ->
    timer:cancel(State#state.timer_ref),
    {stop, normal, State};
handle_cast({set_datetime, Delta}, State) ->
    timer:cancel(State#state.timer_ref),
    fast_forward(State#state{
                   fast_forward=true,
                   delta_seconds = Delta
                  }, qdate:unixtime() + State#state.delta_seconds),
    NewState = State#state{
                 delta_seconds = Delta
                },
    {noreply, next(NewState)};

handle_cast(Msg, State) ->
    ?ERROR("unknown cast msg ~p", [Msg]),
    {noreply, State}.

%% @private
handle_info(do_job_run, #state{
                           mfa = {M, F, A}
                          } = State) ->
    %% ?PRINT("do_job_run  ~p:~p:~p~n", [M, F, A]),
    catch apply(M, F, A),
    {noreply, next(State)};
handle_info(Msg, State) ->
    ?ERROR("unknown info msg ~p", [Msg]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

fast_forward(#state{
                delta_seconds = Delta,
                mfa = {M, F, A},
                spec = Spec
               } = State, Timestamp) ->
    %% ?PRINT("fast_forward Timestamp ~p~n", [qdate:to_date(Timestamp)]),
    %% ?PRINT("fast_forward max ~p~n", [qdate:to_date(Delta + qdate:unixtime())]),

    Timestamp1 = erlcron_time:next(Spec, Timestamp),
    case Timestamp1  =< Delta + qdate:unixtime() of
        true ->
            %% ?PRINT("fast_forward Timestamp1 ~p, Delta ~p~n", [qdate:to_date(Timestamp1), Delta]),
            catch apply(M, F, A),
            fast_forward(State, Timestamp1+1);  
        false ->
            ok
    end.

next(#state{
        delta_seconds = Delta,
        spec = Spec
       } = State) ->
    {MegaSecs, Secs, USec} = os:timestamp(),
    Timestamp = Delta+qdate:to_unixtime({MegaSecs,Secs+1,0}),
    %% ?PRINT("next Timestamp ~p, Delta ~p~n", [qdate:to_date(Timestamp), Delta]),
    Diff = timer:now_diff(qdate:to_now(erlcron_time:next(Spec, Timestamp)), {MegaSecs, Delta+Secs, USec}) div 1000,    
    %%?ERROR("next send after ~p~n", [Diff1]),
    TimerRef = timer:send_after(Diff, self(), do_job_run),
    State#state{
      timer_ref = TimerRef
     }.
