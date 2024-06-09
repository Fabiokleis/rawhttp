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
