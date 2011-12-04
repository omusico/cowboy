%%
%% Adaptor that creates cowboy HTTP requests from the SPDY stream
%%
-module(cowboy_spdy_adaptor).

-include_lib("espdy/include/espdy.hrl").
-include("include/http.hrl").

-export([init/3, closed/2, headers_updated/3, handle_data/2]). %% API

init(_Id, Headers, SpdyOpts) ->
    ?LOG("INIT opts ~p",[SpdyOpts]),
    Dispatch = proplists:get_value(dispatch, SpdyOpts, []),
    Scheme = proplists:get_value(<<"scheme">>, Headers), %% http | https
    Host = proplists:get_value(<<"host">>, Headers),     %% localhost:6121
    Method = proplists:get_value(<<"method">>, Headers), %% GET
    Url = proplists:get_value(<<"url">>, Headers),       %% /
    %% if any of these fields are missing, it's an error
    case Scheme =:= undefined orelse
         Host   =:= undefined orelse
         Method =:= undefined orelse
         Url    =:= undefined of
        true -> 
            {error, not_http};
        false ->
            case  cowboy_dispatcher:split_host(Host) of
                {Host1, HostRaw, undefined} ->
                    Port = 6121; %% TODO spdy port/ssl port?
                {Host1, HostRaw, Port} ->
                    ok
            end,
            {Path, RawPath, Qs} = cowboy_dispatcher:split_path(Url),
            DispatchUrl = [],
            case cowboy_dispatcher:match(Host1, DispatchUrl, Dispatch) of
                    {ok, Handler, Opts, Binds, HostInfo, PathInfo} ->
                        %% Build req that client code will see
                        Req = #http_req{ 
                                socket = undefined,
                                transport = undefined,
                                connection = keepalive,
                                spdy = true,
                                spdy_stream = self(),
                                method = list_to_atom(string:to_upper(binary_to_list(Method))),
                                version = {1,1},
                                peer = {0,0,0,0},
                                host = Host1,
                                raw_host = HostRaw,
                                port = Port,
                                path = Path,
                                raw_path = RawPath,
                                raw_qs = Qs,
                                bindings = Binds,
                                path_info = PathInfo,
                                host_info = HostInfo
                        },
                        handler_init(Req, {Handler, Opts});
                    {error, notfound, host} ->
                        throw({todo, 400});
                    {error, notfound, path} ->
                        throw({todo, 404})
            end
        %%%%ResponseHeaders = [ 
        %%%%    {<<"url">>, <<"http://localhost:6121/">>}, %% url only needed in push streams usually?
        %%%%    {<<"status">>, <<"200 OK">>},
        %%%%    {<<"version">>, <<"HTTP/1.1">>},
        %%%%    {<<"content-type">>, <<"text/plain">>},
        %%%%    {<<"content-length">>, <<"16">>}
        %%%%],
        %%%%Body = <<"Hello SPDY World">>,
        %%%%{ok, ResponseHeaders, Body}
    end.

closed(Reason, _State) ->
    io:format("CLOSED! ~p\n",[Reason]).

headers_updated(_Delta, _NewMergedHeaders, State) ->
    io:format("headers updated with ~p",[_Delta]),
    {ok, State}.

handle_data(Data, State) ->
    io:format("DATA on stream ~p",[Data]),
    {ok, State}.

%%
handler_init(Req, {Handler, Opts}) ->
    try Handler:init({tcp, http}, Req, Opts) of
		{ok, Req2, HandlerState} ->
            try Handler:handle(Req2, HandlerState) of
                {ok, _Req3, _HandlerState2} ->
                    {ok, noreply}
            catch Class:Reason ->
                error_logger:error_msg(
                    "** Handler ~p terminating in handle/2~n"
                    "   for the reason ~p:~p~n"
                    "** Options were ~p~n** Handler state was ~p~n"
                    "** Request was ~p~n** Stacktrace: ~p~n~n",
                    [Handler, Class, Reason, Opts,
                     HandlerState, Req, erlang:get_stacktrace()]),
                handler_terminate(HandlerState, Req2, {Handler,Opts}),
                exit(normal)
            end
%%%%	{loop, Req2, HandlerState} ->
%%%%		handler_before_loop(HandlerState, Req2, State);
%%%%	{loop, Req2, HandlerState, hibernate} ->
%%%%		handler_before_loop(HandlerState, Req2,
%%%%			State#state{hibernate=true});
%%%%	{loop, Req2, HandlerState, Timeout} ->
%%%%		handler_before_loop(HandlerState, Req2,
%%%%			State#state{loop_timeout=Timeout});
%%%%	{loop, Req2, HandlerState, Timeout, hibernate} ->
%%%%		handler_before_loop(HandlerState, Req2,
%%%%			State#state{hibernate=true, loop_timeout=Timeout});
%%%%	{shutdown, Req2, HandlerState} ->
%%%%		handler_terminate(HandlerState, Req2, State);
%%%%	%% @todo {upgrade, transport, Module}
%%%%	{upgrade, protocol, Module} ->
%%%%		Module:upgrade(ListenerPid, Handler, Opts, Req)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in init/3~n"
			"   for the reason ~p:~p~n"
			"** Options were ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts, Req, erlang:get_stacktrace()]),
        throw({todo, 500})
	end.

handler_terminate(HandlerState, Req, {Handler, Opts}) ->
	try
		Handler:terminate(Req#http_req{resp_state=locked}, HandlerState)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in terminate/2~n"
			"   for the reason ~p:~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts,
			 HandlerState, Req, erlang:get_stacktrace()])
	end.
