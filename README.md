rawhttp
=====

Servidor Http puro em erlang!
suporta html, jpeg e png. %% por equanto...

Server
----
An OTP application.

Server env options, listening port and process pool size.
```erlang
[
  {server, [{port, 8123}, {pool, 10}]}
].
```


Build
-----

    $ rebar3 compile


Run
----
    $ rebar shell



inside erlang shell run:
```
observer:start()
```

now you can check server application, supervisor and 10 child processes.

![image](https://github.com/Fabiokleis/rawhttp/assets/66813406/6512333a-ca91-43c0-b919-2727a074c98b)

