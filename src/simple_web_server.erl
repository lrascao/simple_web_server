-module(simple_web_server).
-export([http_start/0]).

-define(PRIVDIR, code:priv_dir(?MODULE)).
-define(COMPILE_OPTS, [{out_dir, ?PRIVDIR ++ "/templates-compiled/"}]).

http_start() ->
    do_erlydtl_start(),
    do_cowboy_start().

do_erlydtl_start() ->
    {ok, Templates} = application:get_env(?MODULE, erlydtl_templates),
    [{ok, _} = erlydtl:compile_file(?PRIVDIR ++ File, Mod, ?COMPILE_OPTS) || {File, Mod} <- Templates].

do_cowboy_start() ->
    {Ip, Port, Workers, Dispatch} = do_cowboy_configure(),
    cowboy:start_http(http, Workers,
        [{ip, Ip}, {port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ).

do_cowboy_configure() ->
    {ok, Ip} = application:get_env(?MODULE, ip_address),
    {ok, Port} = application:get_env(?MODULE, port),
    {ok, Workers} = application:get_env(?MODULE, workers),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", simple_web_server_default_router, <<"index.html">>},
            {"/static/[...]", cowboy_static, {priv_dir, simple_web_server, "static"}},
            {"/v1/health", simple_web_server_health_handler_v1, []},
            {"/v1/account/[...]", simple_web_server_account_api_v1, []},
            {"/v1/config/[...]", simple_web_server_config_api_v1, []},
            {"/[...]", cowboy_static, {priv_dir, simple_web_server, "pages"}}
        ]}
    ]),
    {Ip, Port, Workers, Dispatch}.
