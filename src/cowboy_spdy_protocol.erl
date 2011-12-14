-module(cowboy_spdy_protocol).
-behaviour(cowboy_protocol).
-export([start_link/4]). %% API

%% @doc Start a SPDY protocol process.
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
	NextProtocol =
		case Transport of
			cowboy_ssl_transport ->
				ssl:negotiated_next_protocol(Socket);
			_ ->
				undefined
		end,
	Fallback =
		case NextProtocol of
			<<"spdy/2">> ->
				false;
			_ ->
				proplists:get_value(http_fallback, Opts, false)
		end,
	case Fallback of
		true ->
			cowboy_http_protocol:start_link(ListenerPid, Socket, Transport, Opts);
		false ->
			%% espdy doesn't know about cowboy_*_transport, it uses the socket directly
			M = case Transport of
						cowboy_tcp_transport -> gen_tcp;
						cowboy_ssl_transport -> ssl
					end,
			Opts2 = [ {cowboy_listener_pid, ListenerPid} | Opts ],
			espdy_session:start_link(Socket, M, cowboy_spdy_adaptor, Opts2)
	end.
