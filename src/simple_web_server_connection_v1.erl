-module(simple_web_server_connection_v1).

-behaviour(cowboy_websocket).

%% cowboy websocket api
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-record(state, {
          account_id :: binary(),
          session_pid :: pid(),
          session_monitor :: reference()
     }).

-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% cowboy websocket api
init(#{path_info := [AccountId],
       headers := #{<<"x-app-token">> := Token}} = Req, _) when AccountId =:= Token ->
    {cowboy_websocket, Req, #state{account_id = AccountId},
         #{idle_timeout => 5000}}.

websocket_init(#state{account_id = AccountId} = State0) ->
    % does a session for this account already exist?
    SessionPid =
        case global:whereis_name(AccountId) of
            undefined ->
                {ok, SessionPid0} = simple_web_server_session_sup:start_child(#{connection_pid => self(),
                                                                               account_id => AccountId}),
                lager:debug("created new session for account id ~p at pid ~p",
                            [AccountId, SessionPid0]),
                global:register_name(AccountId, SessionPid0),
                SessionPid0;
            SessionPid0 when is_pid(SessionPid0) ->
                lager:debug("account id ~p session already exists at pid ~p, requesting a reconnect",
                            [AccountId, SessionPid0]),
                ok = gen_server:call(SessionPid0, {reconnect, self()}),
                SessionPid0
        end,

    {ok, State0#state{session_pid = SessionPid,
                      session_monitor = erlang:monitor(process, SessionPid)}}.

websocket_handle(ping, State) ->
    {ok, State};
websocket_handle({text, Data}, State0) ->
    case handle_data(Data, State0) of
        {noreply, State} ->
            {ok, State};
        {reply, Reply, State} ->
            {{text, Reply}, State};
        {error, Error} ->
            lager:error("websocket req failed due to ~p (~p)",
                        [Error, Data]),
            {stop, State0}
    end;
websocket_handle(_Data, State) ->
    lager:debug("unhandled incoming websocket data ~p, state: ~p",
                [_Data, State]),
    {ok, State}.

websocket_info({reply, Reply}, State) ->
    {reply, [{text, jsx:encode(#{reply => Reply})}], State};
websocket_info({disconnect, Reason}, State) ->
    erlang:send_after(2000, self(), stop),
    {reply, {text, jsx:encode(#{disconnect => atom_to_binary(Reason, latin1)})}, State};
websocket_info(stop, State) ->
    {stop, State};
websocket_info({'DOWN', SessionMonitor, process, SessionPid, Reason},
               #state{account_id = AccountId,
                      session_monitor = SessionMonitor,
                      session_pid = SessionPid} = State) ->
    lager:error("account ~p session at pid ~p crashed due to ~p",
                [AccountId, SessionPid, Reason]),
    {stop, State};
websocket_info(_Info, State) ->
    lager:debug("unhandled websocket info ~p, state: ~p",
                [_Info, State]),
    {ok, State}.

terminate(stop, _Req, _State) ->
    ok;
terminate({error, closed}, _Req, _State) ->
    ok;
terminate({remote, 1000, _}, _Req, _State) ->
    ok;
terminate(Reason, Req, State) ->
    lager:error("websocket terminated due to ~p, req ~p, state ~p",
                [Reason, Req, State]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_data(Data :: binary(),
                  State :: state()) -> {reply, binary(), state()} | {noreply, state()} | {error, unknown | failed}.
handle_data(Data, State0) ->
    case jsx:decode(Data, [return_maps, {labels, atom}]) of
        #{req := Req} ->
            case handle_req(Req, State0) of
                {noreply, State} ->
                    {noreply, State};
                {reply, Reply, State} ->
                    {reply, jsx:encode(Reply), State};
                _ ->
                    {error, failed}
            end;
        Unknown ->
            lager:error("websocket req failed (~p)",
                        [Unknown]),
            {error, unknown}
    end.

-spec handle_req(map(), state()) -> {ok, map()}.
handle_req(Req,
           #state{session_pid = SessionPid} = State) ->
    gen_server:cast(SessionPid, Req),
    {noreply, State}.

