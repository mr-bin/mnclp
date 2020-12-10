-module(server_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Port = 8787,
  BufferSize = 1048576, % 1MB. it is required to have some logic around buffer size
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, {buffer, BufferSize}, {mode, binary}, binary]),
  spawn_link(fun empty_listeners/0),

  SupFlags = #{strategy => simple_one_for_one,
                 intensity => 60,
                 period => 3600},
  ChildSpecs = [#{id => socket,
                   start => {server_worker, start_link, [ListenSocket]},
                   restart => temporary,
                   shutdown => 1000,
                   type => worker,
                   modules => [server_worker]
                  }],
  {ok, {SupFlags, ChildSpecs}}.

start_socket() ->
  supervisor:start_child(?MODULE, []).

empty_listeners() ->
  [start_socket() || _ <- lists:seq(1, 20)],
  ok.