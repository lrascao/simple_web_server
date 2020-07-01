-module(simple_web_server_health_handler_v1).

-export([init/2]).

init(Req, Opts) ->
    lager:debug("got health check request"),
    {ok, cowboy_req:reply(200,Req), Opts}.
