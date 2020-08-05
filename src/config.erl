% @copyright Miniclip SA, 2019. All rights reserved.
-module(config).
-behaviour(gen_server).

-compile({no_auto_import,[get/0, get/1]}).

-define(TABLE_NAME, configuration_table).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         get/1, get/2,
         set/2]).

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
-define(LHTTPC_POOL_CONFIG, lhttpc_pool_config).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    current_version = undefined :: undefined | non_neg_integer(),
    table :: ets:tid()
}).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get(Key :: any()) -> {ok, any()} | undefined.
get(Key) ->
    get(Key, undefined).

-spec get(Key :: any(),
          Default :: any()) -> {ok, any()} | undefined.
get(Key, Default) ->
    case catch ets:lookup(?TABLE_NAME, Key) of
        [] ->
            case application:get_env(simple_web_server, Key) of
                undefined ->
                    {ok, Default};
                Other ->
                    Other
            end;
        [{Key, Val}] ->
            {ok, Val};
        Exc ->
            lager:error("failed to get config key ~p due to: ~p",
                       [Key, Exc]),
            undefined
    end.

-spec set(Key :: any(), Value :: any()) -> ok.
set(Key, Val) ->
    gen_server:call(whereis(?SERVER), {set, Key, Val}).

%% ------------------------------------------------------------------
%% gen_server callback Function definitions
%% ------------------------------------------------------------------

init(_) ->
    Table = ets:new(?TABLE_NAME,
                    [set, named_table, protected,
                     {read_concurrency, true}]),

    erlang:send_after(5000, self(), check_config),
    {ok, #state{current_version = undefined,
                table = Table}}.

handle_call({set, Key, Val}, _From,
            #state{table = Table}) ->
    ets:insert(Table, {Key, Val}),
    {reply, ok, Table};
handle_call(Msg, From, State) ->
    lager:notice("unhandled call ~p from ~p for state ~p",
                 [Msg, From, State]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:notice("unhandled cast ~p for state ~p",
                 [Msg, State]),
    {noreply, State}.

handle_info(check_config,
            #state{current_version = CurrentVersion,
                   table = Table} = State0) ->
    {ok, Url} = config:get(configuration_url),
    Version = list_to_integer(string:strip(binary_to_list(download(Url)), both, $\n)),
    State =
        case Version =/= CurrentVersion of
            true ->
                lager:debug("found a new version ~p",
                            [Version]),
                BaseUrl = filename:dirname(Url) ++ "/" ++ integer_to_list(Version),
                FullUrl = BaseUrl ++ "/" ++ "config.json",
                lager:notice("fetching conf. file: ~p",
                             [FullUrl]),
                Config = jsx:decode(download(FullUrl)),
                lager:debug("got config ~p: ~p",
                            [Version, Config]),
                lists:foreach(fun({K, V}) ->
                                ets:insert(Table, {binary_to_atom(K, latin1), V})
                              end,
                              Config),
                State0#state{current_version = Version};
            false ->
                State0
        end,
    erlang:send_after(5000, self(), check_config),
    {noreply, State};
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
-spec download(Url :: nonempty_string()) -> binary().
download(Url) ->
    case lhttpc:request(Url, get, [], "", 5000,
                        [{pool, ?LHTTPC_POOL_CONFIG},
                         {pool_ensure, true}]) of
        {ok, {{200, _Reason}, _Headers, Body}} ->
            Body;
        {ok, {{Code, Reason}, _Headers, _Body}} ->
            erlang:throw({http_error, {Code, Reason, Url}});
        {error, Error} ->
            erlang:throw({http_error, Error, Url})
    end.

