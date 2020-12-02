## Deploying dnsutils

### K8s deployment

```
$ kubectl apply -f deployment.yaml
```

### Usage

```
$ kubectl exec -it <pod-name> -- nslookup redis
```
