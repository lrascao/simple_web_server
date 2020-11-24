-module(simple_web_server_cluster).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/src/inet_dns.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([discover/1]).

% for debug purposes
-export([k8s_reverse_lookup/1]).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------
-type strategy() :: {k8s_dns, SrvRecord :: string()}.

-export_type([strategy/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec discover(strategy()) -> {ok, [node()]}.
discover({k8s_dns, SrvRecord}) ->
    % take the name of the headless k8s service and find the node names
    % behind it
    % This technique requires that the `endpoint_pod_names` option be set
    % in coredns's kubernetes plugin (https://github.com/coredns/coredns/blob/master/plugin/kubernetes/README.md)
    % To update your coredns configMap:
    %   1. add the endpoint_pod_names option 
    %
    %       $ kubectl edit configmap --namespace kube-system coredns
    %
    %       kubernetes cluster.local in-addr.arpa ip6.arpa {
    %           pods insecure
    %           fallthrough in-addr.arpa ip6.arpa
    %           ttl 30
    %           endpoint_pod_names
    %       }
    %   2. restart the coredns so the changes take effect
    %
    %       $ kubectl rollout restart --namespace kube-system deployment/coredns
    %
    {ok, Hosts} = k8s_reverse_lookup(SrvRecord),
    % %% now ping them all, return a list of tuples containing the node atom
    % %% and the pong/pang reply
    Nodes =
        lists:foldl(fun(Host, Acc) ->
            % armed with an ip address we will now perform a reverse dns lookup to
            % find out the node's fully qualified domain name
            case async(fun net_adm:names/1, Host, 5000) of
                {ok, NodeNames} ->
                    lager:debug("names: ~p",
                                [NodeNames]),
                    lists:map(fun({NodeName, _Port}) ->
                                Node = to_atom(NodeName ++ "@" ++ Host),
                                lager:debug("node: ~p",
                                            [Node]),
                                {Node, async(fun net_adm:ping/1, Node, 5000)}
                              end,
                              NodeNames) ++ Acc;
                _ ->
                    lager:error("unable to reach host ~p",
                                [Host]),
                    []
            end
        end,
        [],
        Hosts),
    {ok, Nodes};
discover(_) ->
    {ok, []}.

%% ------------------------------------------------------------------
%% Internal Functions.
%% ------------------------------------------------------------------

to_atom(Name) when is_atom(Name) -> Name;
to_atom(Name) when is_list(Name) -> list_to_atom(Name).

async(Fun, Args, Timeout) ->
    Me = self(),
    Ref = make_ref(),
    spawn(fun () ->
        {ok, _} = timer:kill_after(Timeout),
        Me ! {Ref, Fun(Args)}
          end),
    receive
        {Ref, Result} -> Result
    after Timeout ->
        timeout
    end.

-spec k8s_reverse_lookup(SrvRecord :: string()) -> {ok, [string()]}.
k8s_reverse_lookup(SrvRecord) ->
    % 1. get the FQDN of the headless service
    {ok, #hostent{h_name = SrvRecordFQDN}} = inet:gethostbyname(SrvRecord),
    % 2. ask for SRV records backing up the headless service, this will gives us back, all pod FQDN
    %    names under the headless service subdomain
    {ok, #dns_rec{anlist = Answers}} = inet_res:resolve(SrvRecordFQDN, in, srv),
    % 3. go through each of these records and replace the headless service DNS name component for the
    %    deployment name
    Hosts =
        lists:map(fun(#dns_rr{type = srv,
                              class = in,
                              data = {_Priority, _Weight, _Port, Host}}) ->

                        Host
                  end,
                  Answers),
    {ok, Hosts}.

