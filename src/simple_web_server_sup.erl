% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(simple_web_server_sup).

-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% supervisor behaviour callbacks
%% ------------------------------------------------------------------
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ------------------------------------------------------------------
%% supervisor callback Function definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, { {one_for_one, 10, 10},
           [?CHILD(config, worker),
            ?CHILD(simple_web_server_node_boot, worker),
            ?CHILD(simple_web_server_session_sup, supervisor)]} }.
