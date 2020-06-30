-module(simple_web_server_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    {ok, _} = simple_web_server:http_start(),
    simple_web_server_sup:start_link().

stop(_State) ->
    ok.
