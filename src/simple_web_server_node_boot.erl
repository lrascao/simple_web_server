-module(simple_web_server_node_boot).
-behaviour(gen_server).

-compile({no_auto_import,[get/0, get/1]}).

-define(TABLE_NAME, configuration_table).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

-record(state, {}).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server callback Function definitions
%% ------------------------------------------------------------------

init(_) ->
    gen_server:cast(self(), init),
    {ok, #state{}}.

handle_call(Msg, From, State) ->
    lager:notice("unhandled call ~p from ~p for state ~p",
                 [Msg, From, State]),
    {noreply, State}.

handle_cast(init, State) ->
    discover(application:get_env(simple_web_server, k8s_dns_discovery, undefined)),
    {noreply, State};
handle_cast(Msg, State) ->
    lager:notice("unhandled cast ~p for state ~p",
                 [Msg, State]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:notice("unhandled info ~p for state ~p",
                 [Msg, State]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:notice("terminating due to ~p, state ~p",
                 [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Functions.
%% ------------------------------------------------------------------

discover(SrvRecord) when is_list(SrvRecord) ->
    simple_web_server_cluster:discover({k8s_dns, SrvRecord});
discover(_) ->
    ok.
