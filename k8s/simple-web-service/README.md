## Deploying simple-web-service

### CoreDNS 'endpoint_pod_names' option

This option is needed so we're able to cluster the erlang nodes together.

Edit config and add 'endpoint_pod_names' to the kubernetes plugin option list
  (https://github.com/coredns/coredns/blob/master/plugin/kubernetes/README.md)

```
    $ kubectl edit configmap --namespace kube-system coredns

     kubernetes cluster.local in-addr.arpa ip6.arpa {
         ...
         pods insecure
         fallthrough in-addr.arpa ip6.arpa
         ttl 30
         endpoint_pod_names
         ...
     }
```

Restart coredns
```
    $ kubectl rollout restart --namespace kube-system deployment/coredns
    $ kubectl get pods --namespace=kube-system
```

### Deploying kustomization

```
$ kubectl apply -k .
```

### K8s deployment

```
$ kubectl apply -f deployment.yaml
```
