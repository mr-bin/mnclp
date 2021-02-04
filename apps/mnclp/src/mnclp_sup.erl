-module(mnclp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 10,
                 period => 10},
    ChildSpecs = [#{id => tcp_server_sup,
                   start => {server_sup, start_link, []},
                   restart => permanent,
                   shutdown => 1000,
                   type => supervisor,
                   modules => [server_sup]
                  },
                  #{id => ddb_sup,
                   start => {ddb_sup, start_link, []},
                   restart => permanent,
                   shutdown => 1000,
                   type => worker,
                   modules => [ddb_sup]
                  }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
