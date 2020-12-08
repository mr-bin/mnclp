-module(ddb_worker).
-behaviour(gen_server).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([aws_config/0, aws_ddb_list_tables/1,
  aws_ddb_get_record/3, aws_ddb_put_record/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #{}} | {ok, State :: #{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  Config = aws_config(),
  {ok, #{aws_config => Config}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #{}) ->
  {reply, Reply :: term(), NewState :: #{}} |
  {reply, Reply :: term(), NewState :: #{}, timeout() | hibernate} |
  {noreply, NewState :: #{}} |
  {noreply, NewState :: #{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #{}} |
  {stop, Reason :: term(), NewState :: #{}}).
handle_call({put_data, Data}, _From, State = #{aws_config := Config}) ->
  Table = aws_ddb_list_tables(Config),
  {ok, Resp} = aws_ddb_put_record(Table, Config, Data),
  {reply, {ok, Resp}, State};
handle_call({get_data, Key}, _From, State = #{aws_config := Config}) ->
  Table = aws_ddb_list_tables(Config),
  {ok, Resp} = aws_ddb_get_record(Table, Config, Key),
  {reply, {ok, Resp}, State};
handle_call(_Request, _From, State = #{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #{}) ->
  {noreply, NewState :: #{}} |
  {noreply, NewState :: #{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #{}}).
handle_cast(_Request, State = #{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #{}) ->
  {noreply, NewState :: #{}} |
  {noreply, NewState :: #{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #{}}).
handle_info(_Info, State = #{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #{}) -> term()).
terminate(_Reason, _State = #{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #{}, Extra :: term()) ->
  {ok, NewState :: #{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

aws_config() ->
  Config0 = case os:getenv("USER") of
              "archlinux" -> %% dummy check is application is running on aws instance
                #aws_config{
                  access_key_id = application:get_env(aws_key),
                  secret_access_key = application:get_env(aws_secret),
                  security_token = case os:getenv("AWS_SESSION_TOKEN") of
                                     false -> undefined;
                                     Value -> Value
                                   end,
                  ddb_scheme = "http://",
                  ddb_host = "dynamodb.eu-west-1.amazonaws.com",
                  ddb_port = 80,
                  retry = fun erlcloud_retry:default_retry/1
                };
              _ ->
                #aws_config{
                  access_key_id = "_not_needed_locally_",
                  secret_access_key = "_not_needed_locally_",
                  security_token = "_not_needed_locally_",
                  ddb_scheme = "http://",
                  ddb_host = "localhost",
                  ddb_port = 4566,
                  retry = fun erlcloud_retry:default_retry/1}
            end,
  {ok, Config} = erlcloud_aws:update_config(Config0),
  Config.

aws_ddb_list_tables(Config) ->
  {ok, [Table]} = erlcloud_ddb2:list_tables([], Config),
  Table.

aws_ddb_put_record(Table, Config, Data) ->
  {ok, Resp} = erlcloud_ddb2:put_item(Table, Data, [], Config),
  {ok, Resp}.

aws_ddb_get_record(Table, Config, Key) ->
  {ok, Resp} = erlcloud_ddb2:get_item(Table, Key, [], Config),
  {ok, Resp}.

