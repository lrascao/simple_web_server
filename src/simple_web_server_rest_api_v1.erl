-module(simple_web_server_rest_api_v1).

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

handle_request(<<"PUT">>, <<"/v1/account/", Name/binary>>, Req0) ->
    lager:debug("creating new account for ~p",
                [Name]),
    {ok, UserId} = simple_web_server_db:new_account(Name),
    {ok, UserId, 200, Req0};
handle_request(<<"GET">>, <<"/v1/account/", UserId/binary>>, Req0) ->
    lager:debug("looking up ~p's account",
                [UserId]),
    case simple_web_server_db:read_account(UserId) of
        {ok, #{name := _UserName} = Data} ->
            {ok, jsx:encode(Data), 200, Req0};
        _ ->
            {ok, <<>>, 404, Req0}
    end.
