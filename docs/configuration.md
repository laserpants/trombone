<h1>Basic Configuration</h1>

## Running

#### Ping

```
curl localhost:3010/ping
```

## Unix Signal Handlers

```
kill -SIGHUP `ps -a | awk '/trombone/ {print $1}'`
```

