-module(simple_web_server_default_router).
-export([init/2]).

init(Req, Page) ->
    {ok, ResponseBody} = simple_web_server_templates_index:render(),
    Reply = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], ResponseBody, Req),
    {ok, Reply, Page}.
