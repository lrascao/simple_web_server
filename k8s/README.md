## Setting up a K8s EKS cluster

### Create the EKS cluster with [eksctl](https://eksctl.io/)

```
$ eksctl create cluster --profile <aws-profile> -f eks-cluster.yaml
```

Write kubectl configuration so you can easily switch between different clusters,
the following will add a new entry to `~/.kube/config`

```
$ eksctl utils write-kubeconfig --profile <aws-profile> --region eu-central-1 --cluster=simple-web-service
```

To prevent you from having always to input your MFA token on every kubectl you can
add a `--cache` parameter to the `aws-iam-authenticator` command in kube config:

```
    exec:
      apiVersion: client.authentication.k8s.io/v1alpha1
      args:
      - token
      - -i
      - simple-web-service
      - --cache
      command: aws-iam-authenticator
```

List the available contexts:
```
$ kubectl config get-contexts
```

Use a specific context:
```
$ kubectl config use-context <context-name>
```

### CoreDNS 'endpoint_pod_names' option

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

### CoreDNS rewrite rule

Should you need to re-route DNS queries to some pod, here's what you do:

Show current config:
```
    $ kubectl get configmaps coredns --namespace kube-system --output yaml
```
kubectl edit configmaps coredns --namespace kube-system
    * After `ready` entry
        * rewrite name dynamodb.us-west-2.amazonaws.com localstack.default.svc.cluster.local

Restart coredns
```
    $ kubectl rollout restart --namespace kube-system deployment/coredns
    $ kubectl get pods --namespace=kube-system
```

Validate
```
    $ kubectl exec dnsutils -- nslookup dynamodb.us-west-2.amazonaws.com
```

### Deploying

#### Redis

Check [README.md](https://github.com/lrascao/simple_web_server/blob/feature/clustering/k8s/redis/README.md)

#### Localstack

Check [README.md](https://github.com/lrascao/simple_web_server/blob/feature/clustering/k8s/localstack/README.md)

#### Simple web service

Check [README.md](https://github.com/lrascao/simple_web_server/blob/feature/clustering/k8s/simple-web-service/README.md)
