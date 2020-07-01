-module(simple_web_server_db).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ddb2.hrl").

-export([create_tables/0,
         new_account/1,
         read_account/1]).

-define(FIELD_USER_ID, <<"user_id">>).
-define(FIELD_USER_NAME, <<"name">>).

create_tables() ->
    TableName = <<"account">>,
    Fields = [{?FIELD_USER_ID, s}],
    Primary = ?FIELD_USER_ID,
    ok = 
        case erlcloud_ddb2:create_table(TableName,
                                        Fields,
                                        Primary,
                                        1, 1,
                                        [],
                                        aws_config()) of
            {ok, _} ->
                ok;
            {error, {<<"ResourceInUseException">>, _}} ->
                ok;
            _ ->
                error
        end.

new_account(Name) ->
    UserId = uuid:to_string(uuid:v4()),
    Fields = [{?FIELD_USER_ID, UserId},
              {?FIELD_USER_NAME, Name}],
    TableName = <<"account">>,
    erlcloud_ddb2:put_item(TableName,
                           Fields,
                           [],
                           aws_config()),
    {ok, UserId}.

read_account(UserId) ->
    case erlcloud_ddb2:get_item(<<"account">>,
                                [{?FIELD_USER_ID, {s, UserId}}],
                                [consistent_read, {attributes_to_get, [?FIELD_USER_NAME]}],
                                aws_config()) of
        {ok, []} ->
            {error, not_found};
        {ok, [{?FIELD_USER_NAME, UserName}]} ->
            {ok, #{user_id => UserId,
                   name => UserName}}
    end.

aws_config() ->
    Port = application:get_env(simple_web_server, ddb_port, 8000),
    Host = application:get_env(simple_web_server, ddb_host, "localhost"),
    #aws_config{
        access_key_id = "simple-docker-web-server",
        secret_access_key = "simple-docker-web-server",
        ddb_host = Host,
        ddb_port = Port,
        ddb_scheme = "http://",
        http_client = lhttpc
    }.

