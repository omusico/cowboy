-module(my_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-record(state, {}).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    io:format("REQ: ~p\n",[Req]),
    {ok, Req2} = cowboy_http_req:reply(200, [], <<"Hello World!">>, Req),
    io:format("REQ2: ~p\n",[Req2]),
    {ok, Req2, State}.

terminate(Req, State) ->
    ok.
