# Simple Mirroring Proxy with NGINX

How to run

* Run docker in the **host network mode** to be able to access your real localhost.

```shell script
docker run -v <PATH TO PROJECT>/mirror-proxy/nginx.conf:/etc/nginx/nginx.conf:ro --name my-nginx-proxy --network host -d nginx 
```

* Have some endpoints listening on port 8081 and 8082 of localhost to HTTP requests.