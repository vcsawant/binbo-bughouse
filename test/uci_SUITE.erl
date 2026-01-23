%% Copyright (c) 2019-2025, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(uci_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include("binbo_test_lib.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([
    uci_test_engine_local/1,
    uci_test_engine_tcp/1,
    uci_test_failed_connection/1
]).

%% all/0
all() -> [{group, all_uci_tests}].

%% groups/0
groups() ->
    [{all_uci_tests, [parallel], [
        uci_test_engine_local,
        uci_test_engine_tcp,
        uci_test_failed_connection
    ]}].


%% init_per_suite/1
init_per_suite(Config) ->
    ok = binbo_test_lib:all_group_testcases_exported(?MODULE),
    {ok, _} = binbo_bughouse:start(),
    Config.

%% end_per_suite/1
end_per_suite(_Config) ->
    ok = binbo_bughouse:stop(),
    ok.

%% init_per_testcase/2
init_per_testcase(uci_test_engine_local, Config) ->
    EnginePath = os:getenv("BINBO_UCI_ENGINE_PATH"),
    case validate_engine_file_path(EnginePath) of
        ok ->
            [{engine_path, EnginePath} | Config];
        {error, Reason} ->
            {skip, {Reason, EnginePath}}
    end;
init_per_testcase(uci_test_engine_tcp, Config) ->
    EnvEngineHost = os:getenv("BINBO_UCI_ENGINE_HOST"),
    EnvEnginePort = os:getenv("BINBO_UCI_ENGINE_PORT"),
    case {EnvEngineHost, EnvEnginePort} of
        {[_|_], [_|_]} ->
            EnginePort = erlang:list_to_integer(EnvEnginePort),
            EnginePath = {EnvEngineHost, EnginePort, 5000},
            [{engine_path, EnginePath} | Config];
        {_, _} ->
            {skip, {{engine_host, EnvEngineHost}, {engine_port, EnvEnginePort}}}
    end;
init_per_testcase(_TestCase, Config) ->
    Config.

%% end_per_testcase/2
end_per_testcase(_TestCase, _Config) ->
    ok.

%% uci_test_engine_local/1
uci_test_engine_local(Config) ->
    _ = uci_test_play_game(Config),
    ok.

%% uci_test_engine_tcp/1
uci_test_engine_tcp(Config) ->
    _ = uci_test_play_game(Config),
    ok.

%% uci_test_play_game/1
uci_test_play_game(Config) ->
    EnginePath = ?value(engine_path, Config),
    InitialFen = binbo_fen:initial(),

    % Start new process for the game
    {ok, Pid} = binbo_bughouse:new_server(),

    % Start new game with engine (initial FEN)
    {ok, continue} = binbo_bughouse:new_uci_game(Pid, #{engine_path => EnginePath}),

    % Start new game with engine (given FEN)
    {ok, continue} = binbo_bughouse:new_uci_game(Pid, #{engine_path => EnginePath, fen => InitialFen}),

    % Best move
    {ok, BestMove1} = binbo_bughouse:uci_bestmove(Pid),
    {ok, BestMove2} = binbo_bughouse:uci_bestmove(Pid, #{}),
    {ok, BestMove3} = binbo_bughouse:uci_bestmove(Pid, #{movetime => 100}),
    true = erlang:is_binary(BestMove1),
    true = erlang:is_binary(BestMove2),
    true = erlang:is_binary(BestMove3),

    % Play
    {ok, continue, EngineMove1} = binbo_bughouse:uci_play(Pid, #{movetime => 100}, <<"e2e4">>),
    {ok, continue, EngineMove2} = binbo_bughouse:uci_play(Pid, #{}),
    true = erlang:is_binary(EngineMove1),
    true = erlang:is_binary(EngineMove2),

    % Change position back to initial, make moves and sync
    {ok, continue} = binbo_bughouse:uci_set_position(Pid, InitialFen),
    {ok, continue} = binbo_bughouse:move(Pid, <<"e2e4">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"e7e5">>),
    ok = binbo_bughouse:uci_sync_position(Pid),

    % Set UCI mode on
    ok = binbo_bughouse:uci_mode(Pid),

    % Set default message handler
    ok = binbo_bughouse:set_uci_handler(Pid, default),
    % Send command
    ok = binbo_bughouse:uci_command_call(Pid, "uci"),

    % Set custom message handler
    Self = erlang:self(),
    ok = binbo_bughouse:set_uci_handler(Pid, fun(Msg) -> Self ! Msg end),
    ok = binbo_bughouse:uci_command_cast(Pid, "uci"),
    ok = binbo_bughouse:set_uci_handler(Pid, undefined),

    % Set invalid message handler
    {error, invalid_handler} = binbo_bughouse:set_uci_handler(Pid, test_invalid),
    {error, bad_function_arity} = binbo_bughouse:set_uci_handler(Pid, fun(Msg1, Msg2) -> io:format("~p ~p", [Msg1, Msg2]) end),

    % Send invalid command
    ok = binbo_bughouse:uci_command_call(Pid, "invalid command"),
    ok = binbo_bughouse:uci_command_cast(Pid, "invalid command"),

    % Send 'quit'
    ok = binbo_bughouse:uci_command_call(Pid, "quit"),
    _ = timer:sleep(500),
    {error,no_uci_connection} = binbo_bughouse:uci_command_call(Pid, "uci"),
    ok = binbo_bughouse:uci_command_cast(Pid, "uci"),

    % Stop the game process
    ok = binbo_bughouse:stop_server(Pid),
    ok.

%% uci_test_failed_connection/1
uci_test_failed_connection(_Config) ->
    % Start new process for the game
    {ok, Pid} = binbo_bughouse:new_server(),

    % Connect to local engine
    {error,{uci_connection_failed,enoent}} = binbo_bughouse:new_uci_game(Pid, #{
        engine_path => "/usr/local/bin/stockfish-test-0123456789"
    }),

    % Connect over TCP
    {error,{uci_connection_failed,nxdomain}} = binbo_bughouse:new_uci_game(Pid, #{
        engine_path => {"localhost-test-0123456789", 9011, 1000}
    }),

    % Send commands
    ok = binbo_bughouse:uci_command_cast(Pid, "uci"),
    {error,no_uci_connection} = binbo_bughouse:uci_mode(Pid),
    {error,no_uci_connection} = binbo_bughouse:uci_command_call(Pid, "uci"),

    % Stop the game process
    ok = binbo_bughouse:stop_server(Pid),
    ok.

%% validate_engine_file_path/1
validate_engine_file_path([_|_] = EnginePath) ->
    case file:read_file_info(EnginePath) of
        {ok, #file_info{type = regular}} ->
            ok;
        {ok, _} ->
            {error, not_regular_file};
        {error, Reason} ->
            {error, Reason}
    end;
validate_engine_file_path(_) ->
    {error, no_egine_path_provided}.
