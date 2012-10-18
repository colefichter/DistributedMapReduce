-module(slave).
-compile([export_all]).

start() ->
    net_adm:ping(mrsmaster@icosahedron),
    worker:new(),
    worker:new(),
    worker:new(),
    bootstrap:seed().
