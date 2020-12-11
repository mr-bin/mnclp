-module(ddb_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([get_ddb_worker_pid/0]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

get_ddb_worker_pid() ->
  Pids = [Pid || { _ , Pid ,worker,[ddb_worker]} <- supervisor:which_children(ddb_sup)],
  Index = rand:uniform(length(Pids)),
  lists:nth(Index,Pids).

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([]) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 10,
    period => 10},

  ChildSpecs = [child_spec(Suffix) || Suffix <- lists:seq(1, 20)],

  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

child_spec(Suffix0) ->
  Suffix = integer_to_binary(Suffix0),
  ChildId = binary_to_atom(<<"ddb_worker_", Suffix/binary>>, utf8),
  #{id => ChildId,
    start => {ddb_worker, start_link, [ChildId]},
    restart => permanent,
    shutdown => 1000,
    type => worker,
    modules => [ddb_worker]
  }.