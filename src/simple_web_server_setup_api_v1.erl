-module(simple_web_server_setup_api_v1).

-behaviour(cowboy_handler).

% behaviour callbacks
-export([init/2,
         terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req0, State) ->
    lager:debug("got api request: ~p",
                [Req0]),
    {ok, RespBody, RespCode, Req1} = handle_request(cowboy_req:method(Req0),
                                          cowboy_req:path(Req0),
                                          Req0),
    Req2 = cowboy_req:set_resp_body(RespBody, Req1), 
    {ok, cowboy_req:reply(RespCode, Req2), State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_request(<<"POST">>, <<"/v1/setup/create_tables">>, Req0) ->
    lager:debug("creating ddb tables"),
    simple_web_server_db:create_tables(),
    {ok, <<>>, 200, Req0}.
