-module(mnclp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([get_ddb_worker_pid/0]).

-define(SERVER, ?MODULE).

get_ddb_worker_pid() ->
  [{ddb_worker, Pid, _, _}| _] = supervisor:which_children(mnclp_sup),
  Pid.


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => tcp_server_sup,
                   start => {server_sup, start_link, []},
                   restart => temporary,
                   shutdown => 1000,
                   type => supervisor,
                   modules => [server_sup]
                  },
                  #{id => ddb_worker,
                   start => {ddb_worker, start_link, []},
                   restart => temporary,
                   shutdown => 1000,
                   type => worker,
                   modules => [ddb_worker]
                  }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
