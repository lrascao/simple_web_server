-module(simple_web_server_config_api_v1).

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

handle_request(<<"GET">>, <<"/v1/config/", Key/binary>>, Req0) ->
    lager:debug("obtaining config key ~p",
                [Key]),
    {ok, Value} = config:get(binary_to_atom(Key, latin1), undefined),
    {ok, jsx:encode(#{Key => Value}), 200, Req0}.
