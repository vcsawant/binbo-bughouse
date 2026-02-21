%%%------------------------------------------------------------------------------
%%% @doc
%%% Common Test suite for checkmate rules in Bughouse mode.
%%% Verifies that king capture is illegal, check validation is enforced,
%%% and checkmate properly ends the game.
%%% @end
%%%------------------------------------------------------------------------------
-module(king_capture_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    king_capture_illegal_bughouse_test/1,
    checkmate_white_wins_bughouse_test/1,
    checkmate_black_wins_bughouse_test/1,
    king_cannot_move_into_check_bughouse_test/1,
    pinned_piece_bughouse_test/1
]).

%%%=============================================================================
%%% CT callbacks
%%%=============================================================================

all() ->
    [
        king_capture_illegal_bughouse_test,
        checkmate_white_wins_bughouse_test,
        checkmate_black_wins_bughouse_test,
        king_cannot_move_into_check_bughouse_test,
        pinned_piece_bughouse_test
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(binbo_bughouse),
    Config.

end_per_suite(_Config) ->
    application:stop(binbo_bughouse),
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, Pid} = binbo_bughouse:new_server(),
    [{pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    Pid = ?config(pid, Config),
    binbo_bughouse:stop_server(Pid),
    ok.

%%%=============================================================================
%%% Test cases
%%%=============================================================================

%% @doc Verify king capture is illegal in bughouse mode
king_capture_illegal_bughouse_test(Config) ->
    Pid = ?config(pid, Config),
    %% White queen on e5 can see black king on e8, but capturing it is illegal
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/8/8/4Q3/8/8/8/4K3 w - - 0 1">>,
        #{mode => bughouse}),

    %% Attempting to capture king should fail
    {error, {{invalid_move, king_capture}, <<"e5e8">>}} =
        binbo_bughouse:move(Pid, <<"e5e8">>),
    ok.

%% @doc Verify checkmate detection: white delivers back-rank mate
checkmate_white_wins_bughouse_test(Config) ->
    Pid = ?config(pid, Config),
    %% Back-rank mate position: white rook delivers mate on 8th rank
    %% Black king on g8, pawns on f7,g7,h7 block escape
    %% White rook on a1 can go to a8 for mate
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"6k1/5ppp/8/8/8/8/8/R3K3 w - - 0 1">>,
        #{mode => bughouse}),

    %% Deliver checkmate
    {ok, {checkmate, white_wins}} = binbo_bughouse:move(Pid, <<"a1a8">>),

    %% Verify game status
    {ok, {checkmate, white_wins}} = binbo_bughouse:game_status(Pid),
    ok.

%% @doc Verify checkmate detection: black delivers mate
checkmate_black_wins_bughouse_test(Config) ->
    Pid = ?config(pid, Config),
    %% Black rook delivers back-rank mate
    %% White king on g1, pawns on f2,g2,h2 block escape
    %% Black king on e8, black rook on a8 → Ra1#
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"r3k3/8/8/8/8/8/5PPP/6K1 b - - 0 1">>,
        #{mode => bughouse}),

    %% Deliver checkmate (rook covers entire 1st rank)
    {ok, {checkmate, black_wins}} = binbo_bughouse:move(Pid, <<"a8a1">>),

    %% Verify game status
    {ok, {checkmate, black_wins}} = binbo_bughouse:game_status(Pid),
    ok.

%% @doc Verify king cannot move into check in bughouse mode
king_cannot_move_into_check_bughouse_test(Config) ->
    Pid = ?config(pid, Config),
    %% Black rook on d8 attacks d1
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"3rk3/8/8/8/8/8/8/4K3 w - - 0 1">>,
        #{mode => bughouse}),

    %% King cannot move into check
    {error, {{invalid_move, own_king_in_check}, <<"e1d1">>}} =
        binbo_bughouse:move(Pid, <<"e1d1">>),
    ok.

%% @doc Verify pinned piece cannot move in bughouse mode
pinned_piece_bughouse_test(Config) ->
    Pid = ?config(pid, Config),
    %% White bishop on d2 is pinned by black rook on a5 (pin along a5-e1 diagonal? No...)
    %% Better: white rook on e4 pinned to white king on e1 by black rook on e8
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4r1k1/8/8/8/4R3/8/8/4K3 w - - 0 1">>,
        #{mode => bughouse}),

    %% White rook on e4 is pinned by black rook on e8 to white king on e1
    %% Moving the rook off the e-file should be illegal
    {error, {{invalid_move, own_king_in_check}, <<"e4d4">>}} =
        binbo_bughouse:move(Pid, <<"e4d4">>),

    %% But moving along the pin line should be legal
    {ok, continue} = binbo_bughouse:move(Pid, <<"e4e8">>),
    ok.
