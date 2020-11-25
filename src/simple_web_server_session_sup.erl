% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(simple_web_server_session_sup).

-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         start_child/1]).

%% ------------------------------------------------------------------
%% supervisor behaviour callbacks
%% ------------------------------------------------------------------

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
   supervisor:start_child(?MODULE, [Args]).

%% ------------------------------------------------------------------
%% supervisor callback Function definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, { {simple_one_for_one, 60, 3600},
           [?CHILD(simple_web_server_session, worker)]} }.
