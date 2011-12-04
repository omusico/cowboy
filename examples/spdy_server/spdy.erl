-module(spdy).
-compile(export_all).

start() ->
    application:start(cowboy),
    Dispatch = [
            %% {Host, list({Path, Handler, Opts})}
                {'_', [{'_', my_handler, []}]}
               ],
    start_http(Dispatch),
    start_spdy(Dispatch),
    ok.

start_http(Dispatch) ->
    cowboy:start_listener(http, 3,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ).

start_spdy(Dispatch) ->
    cowboy:start_listener(spdy, 3,
        cowboy_tcp_transport, [{port, 6121}],
        cowboy_spdy_protocol, [{dispatch, Dispatch}]
    ).
