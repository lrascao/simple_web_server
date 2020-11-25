-module(simple_web_server_account_api_v1).

-behaviour(cowboy_handler).

% behaviour callbacks
-export([init/2,
         terminate/3]).

-define(DEFAULT_REDIS_DATABASE, 0).
-define(DEFAULT_REDIS_PASSWORD, "").

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
    % cache the newly inserted data 
    {ok, TTL} = config:get(redis_ttl, 10),
    {ok, RedisClient} = new_eredis_client(),
    eredis:q(RedisClient, ["SETEX", UserId, TTL,
                           jsx:encode(#{user_id => UserId,
                                        name => Name})]),
    eredis:stop(RedisClient),
    {ok, UserId, 200, Req0};
handle_request(<<"GET">>, <<"/v1/account/", UserId/binary>>, Req0) ->
    lager:debug("looking up ~p's account",
                [UserId]),
    {ok, RedisClient} = new_eredis_client(),
    % hit the cache first
    case eredis:q(RedisClient, ["GET", UserId]) of
        {ok, undefined} ->
            lager:debug("cache miss on key ~p, hitting db",
                        [UserId]),
            case simple_web_server_db:read_account(UserId) of
                {ok, #{name := Name} = Data} ->
                    % cache the newly fetched data
                    {ok, TTL} = config:get(redis_ttl, 10),
                    eredis:q(RedisClient, ["SETEX", UserId, TTL,
                                           jsx:encode(#{user_id => UserId,
                                                        name => Name})]),
                    eredis:stop(RedisClient),
                    {ok, jsx:encode(Data), 200, Req0};
                _ ->
                    eredis:stop(RedisClient),
                    {ok, <<>>, 404, Req0}
            end;
        {ok, Data} ->
            eredis:stop(RedisClient),
            lager:debug("cache hit on key ~p",
                        [UserId]),
            % no need to jsx encode here, it was already encoded when
            % cached
            {ok, Data, 200, Req0}
    end.

-spec new_eredis_client() -> {ok, pid()}.
new_eredis_client() ->
    {ok, Host} = application:get_env(eredis, host),
    {ok, Port} = application:get_env(eredis, port),
    Database = application:get_env(eredis, database,
                                   ?DEFAULT_REDIS_DATABASE),
    Password = application:get_env(eredis, password,
                                   ?DEFAULT_REDIS_PASSWORD),
    eredis:start_link(tcp, Host, Port, Database, Password).
