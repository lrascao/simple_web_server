-module(simple_web_server_session).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server callback Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
          account_id :: binary(),
          connection_pid :: pid(),
          connection_monitor :: reference()
}).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(map()) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

%% ------------------------------------------------------------------
%% gen_server callback Function definitions
%% ------------------------------------------------------------------

init([#{connection_pid := ConnectionPid,
        account_id := AccountId}]) ->
    {ok, #state{connection_pid = ConnectionPid,
                connection_monitor = erlang:monitor(process, ConnectionPid),
                account_id = AccountId}}.

handle_call({reconnect, ConnectionPid}, _From,
            #state{account_id = AccountId,
                   connection_pid = OldConnectionPid,
                   connection_monitor = OldConnectionMonitor} = State0) ->
    lager:info("reconnecting account ~p from old connection ~p to new one ~p",
               [AccountId, OldConnectionPid, ConnectionPid]),
    % demonitor the old connection
    erlang:demonitor(OldConnectionMonitor, [flush]),
    % inform the old connection to disconnect it's client
    OldConnectionPid ! {disconnect, reconnected},
    % and monitor the new one
    {reply, ok, State0#state{connection_pid = ConnectionPid,
                             connection_monitor = erlang:monitor(process, ConnectionPid)}};
handle_call(Msg, From, State) ->
    lager:notice("unhandled call ~p from ~p for state ~p",
                 [Msg, From, State]),
    {noreply, State}.

handle_cast(#{type := <<"version">>},
            #state{connection_pid = ConnectionPid} = State) ->
    {simple_web_server, _, Version} = lists:keyfind(simple_web_server, 1, application:which_applications()),
    ConnectionPid ! {reply, #{type => version,
                              version => list_to_binary(Version)}},
    {noreply, State};
handle_cast(Msg, State) ->
    lager:notice("unhandled cast ~p for state ~p",
                 [Msg, State]),
    {noreply, State}.

handle_info({'DOWN', ConnectMonitor, process, ConnectionPid, Reason},
            #state{account_id = AccountId,
                   connection_pid = ConnectionPid,
                   connection_monitor = ConnectMonitor} = State) ->
    lager:debug("account ~p connection at pid ~p exited due to ~p",
                [AccountId, ConnectionPid, Reason]),
    {stop, normal, State};
handle_info(Msg, State) ->
    lager:notice("unhandled info ~p for state ~p",
                 [Msg, State]),
    {noreply, State}.

terminate(Reason, #state{account_id = AccountId} = State) ->
    ok = global:unregister_name(AccountId),
    lager:notice("terminating due to ~p, state ~p",
                 [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Functions.
%% ------------------------------------------------------------------

