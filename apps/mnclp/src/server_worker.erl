-module(server_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("kv_pb.hrl").


start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
  gen_server:cast(self(), accept),
  {ok, #{socket => Socket}}.

handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(accept, S = #{socket := ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  server_sup:start_socket(),
  send(AcceptSocket, "Send some data", []),
  {noreply, S#{socket => AcceptSocket, partial_data => undef}};
handle_cast(E, S) ->
  io:format("unexpected: ~p~n", [E]),
  {noreply, S}.

handle_info({tcp, _Socket, Str}, S = #{socket := Socket, partial_data := PartialData0}) ->
  RecordOrError = case PartialData0 of
                    undef -> try
                               kv_pb:decode_msg(Str, req_envelope)
                             catch _:_ -> error
                             end;
                    _ -> try
                           kv_pb:decode_msg(<<PartialData0/binary, Str/binary>>, req_envelope)
                         catch _:_ -> error
                         end
                  end,
  case RecordOrError of
    error ->
      PartialData = case PartialData0 of
                      undef -> Str;
                      _ -> <<PartialData0/binary, Str/binary>>
                    end,
      inet:setopts(Socket, [{active, once}]),
      {noreply, S#{partial_data => PartialData}};
    Encoded ->
      Resp = process_request(Encoded),
      send(Socket, Resp),
      {noreply, S#{partial_data => undef}}
  end;
handle_info({tcp_closed, _Socket}, S) ->
  {stop, normal, S};
handle_info({tcp_error, _Socket, _}, S) ->
  {stop, normal, S};
handle_info(E, S) ->
  io:format("unexpected: ~p~n", [E]),
  {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(_Reason, _State) ->
  io:format("terminate reason: ~p~n", [_Reason]).

send(Socket, Str) ->
  ok = gen_tcp:send(Socket, Str),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str ++ "~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

process_request(#req_envelope{type = set_request_t} = Req) ->
  Key = Req#req_envelope.set_req#set_request.req#data.key,
  Value = Req#req_envelope.set_req#set_request.req#data.value,

  DdbPid = ddb_sup:get_ddb_worker_pid(),
  Resp = case gen_server:call(DdbPid, {put_data, [{<<"key">>, {b, Key}},
    {<<"value">>, Value},
    {<<"vsn">>, <<"1">>}]}) of
           {ok, _} -> ok;
           {error, _} -> internal
         end,
  kv_pb:encode_msg(#req_envelope{type = set_response_t,
    set_resp = #set_response{error = Resp}});
process_request(#req_envelope{type = get_request_t} = Req) ->
  Key = Req#req_envelope.get_req#get_request.key,

  DdbPid = ddb_sup:get_ddb_worker_pid(),
  Resp = case gen_server:call(DdbPid, {get_data, {<<"key">>, {b, Key}}}) of
           {ok, []} -> #get_response{error = not_found};
           {ok, Item} ->
             Key = proplists:get_value(<<"key">>, Item),
             Value = proplists:get_value(<<"value">>, Item),
             #get_response{error = ok, req = #data{key = Key, value = Value}};
           {error, _} -> #get_response{error = internal}
         end,
  kv_pb:encode_msg(#req_envelope{type = get_response_t, get_resp = Resp}).
