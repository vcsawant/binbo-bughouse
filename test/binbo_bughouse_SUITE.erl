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

%%%------------------------------------------------------------------------------
%%%   Bughouse Chess Tests
%%%------------------------------------------------------------------------------

-module(binbo_bughouse_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("binbo_test_lib.hrl").

-export([all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Reserve management tests
-export([
    test_add_piece_to_reserve/1,
    test_drop_decrements_reserve/1,
    test_add_multiple_pieces/1,
    test_reserve_count_accuracy/1
]).

%% Piece drops tests
-export([
    test_valid_piece_drop/1,
    test_drop_pawn_valid_squares/1,
    test_drop_knight/1,
    test_drop_bishop/1,
    test_drop_rook/1,
    test_drop_queen/1,
    test_cannot_drop_pawn_on_rank_1/1,
    test_cannot_drop_pawn_on_rank_8/1,
    test_cannot_drop_on_occupied_square/1,
    test_cannot_drop_without_reserve/1,
    test_drop_notation_parsing/1
]).

%% Legal drops tests
-export([
    test_all_legal_drops_empty_reserves/1,
    test_all_legal_drops_with_pawns/1,
    test_all_legal_drops_with_knights/1,
    test_all_legal_drops_multiple_pieces/1,
    test_can_drop_valid_square/1,
    test_can_drop_invalid_square/1,
    test_can_drop_no_piece_in_reserve/1
]).

%% Checkmate tests
-export([
    test_checkmate_ends_game/1,
    test_checkmate_returns_winner/1,
    test_checkmate_white_wins/1,
    test_checkmate_black_wins/1,
    test_king_capture_illegal_in_bughouse_mode/1
]).

%% Bughouse mode validation tests
-export([
    test_king_in_check_blocked_bughouse_mode/1,
    test_king_in_check_blocked_standard_mode/1,
    test_move_into_check_blocked_bughouse_mode/1,
    test_move_into_check_standard_mode/1
]).

%% FEN with reserves tests
-export([
    test_parse_fen_no_reserves/1,
    test_parse_fen_white_reserves_only/1,
    test_parse_fen_black_reserves_only/1,
    test_parse_fen_both_reserves/1,
    test_parse_fen_multiple_pieces/1,
    test_generate_fen_no_reserves/1,
    test_generate_fen_with_reserves/1,
    test_fen_round_trip_with_reserves/1,
    test_fen_round_trip_preserves_mode/1
]).

%% Mode persistence tests
-export([
    test_default_mode_is_standard/1,
    test_bughouse_mode_initialization/1,
    test_standard_mode_initialization/1,
    test_mode_survives_fen_export_import/1,
    test_mode_in_game_state/1
]).

%% Edge cases tests
-export([
    test_drop_after_game_over/1,
    test_drop_after_checkmate/1,
    test_castling_in_bughouse_mode/1,
    test_en_passant_in_bughouse_mode/1,
    test_promotion_in_bughouse_mode/1,
    test_multiple_drops_in_sequence/1,
    test_drop_creates_check/1,
    test_drop_creates_checkmate/1,
    test_drop_blocks_check/1,
    test_drop_while_in_check_non_blocking/1,
    test_empty_reserves_not_checkmate_bughouse/1
]).

%% Backward compatibility tests
-export([
    test_standard_mode_checkmate_works/1,
    test_standard_mode_stalemate_works/1,
    test_standard_mode_like_original_binbo/1,
    test_standard_mode_king_cannot_be_captured/1,
    test_standard_mode_king_in_check_validation/1,
    test_standard_mode_rejects_too_many_pawns/1
]).

%% BFEN compliance tests
-export([
    test_bfen_single_bracket_reserves/1,
    test_bfen_empty_reserves_bughouse/1,
    test_bfen_standard_mode_no_brackets/1,
    test_bfen_canonical_reserve_ordering/1,
    test_bfen_promoted_piece_in_fen/1,
    test_bfen_parse_fen_with_tilde/1,
    test_bfen_promoted_round_trip/1,
    test_bfen_backward_compat_two_bracket_parse/1
]).

%% Integration tests
-export([
    test_full_game_workflow/1,
    test_move_capture_add_drop_sequence/1,
    test_reserves_across_multiple_moves/1,
    test_fen_load_with_reserves_and_play/1
]).

%%%------------------------------------------------------------------------------
%%%   CT Callbacks
%%%------------------------------------------------------------------------------

all() ->
    [
        {group, reserve_management},
        {group, piece_drops},
        {group, legal_drops},
        {group, checkmate},
        {group, bughouse_mode_validation},
        {group, fen_with_reserves},
        {group, mode_persistence},
        {group, edge_cases},
        {group, backward_compatibility},
        {group, bfen_compliance},
        {group, integration}
    ].

groups() ->
    [
        {reserve_management, [parallel], [
            test_add_piece_to_reserve,
            test_drop_decrements_reserve,
            test_add_multiple_pieces,
            test_reserve_count_accuracy
        ]},

        {piece_drops, [parallel], [
            test_valid_piece_drop,
            test_drop_pawn_valid_squares,
            test_drop_knight,
            test_drop_bishop,
            test_drop_rook,
            test_drop_queen,
            test_cannot_drop_pawn_on_rank_1,
            test_cannot_drop_pawn_on_rank_8,
            test_cannot_drop_on_occupied_square,
            test_cannot_drop_without_reserve,
            test_drop_notation_parsing
        ]},

        {legal_drops, [parallel], [
            test_all_legal_drops_empty_reserves,
            test_all_legal_drops_with_pawns,
            test_all_legal_drops_with_knights,
            test_all_legal_drops_multiple_pieces,
            test_can_drop_valid_square,
            test_can_drop_invalid_square,
            test_can_drop_no_piece_in_reserve
        ]},

        {checkmate, [parallel], [
            test_checkmate_ends_game,
            test_checkmate_returns_winner,
            test_checkmate_white_wins,
            test_checkmate_black_wins,
            test_king_capture_illegal_in_bughouse_mode
        ]},

        {bughouse_mode_validation, [parallel], [
            test_king_in_check_blocked_bughouse_mode,
            test_king_in_check_blocked_standard_mode,
            test_move_into_check_blocked_bughouse_mode,
            test_move_into_check_standard_mode
        ]},

        {fen_with_reserves, [parallel], [
            test_parse_fen_no_reserves,
            test_parse_fen_white_reserves_only,
            test_parse_fen_black_reserves_only,
            test_parse_fen_both_reserves,
            test_parse_fen_multiple_pieces,
            test_generate_fen_no_reserves,
            test_generate_fen_with_reserves,
            test_fen_round_trip_with_reserves,
            test_fen_round_trip_preserves_mode
        ]},

        {mode_persistence, [parallel], [
            test_default_mode_is_standard,
            test_bughouse_mode_initialization,
            test_standard_mode_initialization,
            test_mode_survives_fen_export_import,
            test_mode_in_game_state
        ]},

        {edge_cases, [parallel], [
            test_drop_after_game_over,
            test_drop_after_checkmate,
            test_castling_in_bughouse_mode,
            test_en_passant_in_bughouse_mode,
            test_promotion_in_bughouse_mode,
            test_multiple_drops_in_sequence,
            test_drop_creates_check,
            test_drop_creates_checkmate,
            test_drop_blocks_check,
            test_drop_while_in_check_non_blocking,
            test_empty_reserves_not_checkmate_bughouse
        ]},

        {backward_compatibility, [parallel], [
            test_standard_mode_checkmate_works,
            test_standard_mode_stalemate_works,
            test_standard_mode_like_original_binbo,
            test_standard_mode_king_cannot_be_captured,
            test_standard_mode_king_in_check_validation,
            test_standard_mode_rejects_too_many_pawns
        ]},

        {bfen_compliance, [parallel], [
            test_bfen_single_bracket_reserves,
            test_bfen_empty_reserves_bughouse,
            test_bfen_standard_mode_no_brackets,
            test_bfen_canonical_reserve_ordering,
            test_bfen_promoted_piece_in_fen,
            test_bfen_parse_fen_with_tilde,
            test_bfen_promoted_round_trip,
            test_bfen_backward_compat_two_bracket_parse
        ]},

        {integration, [parallel], [
            test_full_game_workflow,
            test_move_capture_add_drop_sequence,
            test_reserves_across_multiple_moves,
            test_fen_load_with_reserves_and_play
        ]}
    ].

init_per_suite(Config) ->
    ok = binbo_test_lib:all_group_testcases_exported(?MODULE),
    {ok, _} = binbo_bughouse:start(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, Pid} = binbo_bughouse:new_server(),
    [{pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    Pid = get_pid(Config),
    binbo_bughouse:stop_server(Pid),
    ok.

%%%------------------------------------------------------------------------------
%%%   Helper Functions
%%%------------------------------------------------------------------------------

get_pid(Config) ->
    ?value(pid, Config).

%% Helper to verify reserves match expected values
assert_reserves(Pid, ExpectedWhite, ExpectedBlack) ->
    {ok, Reserves} = binbo_bughouse:get_reserves(Pid),
    ExpectedWhite = maps:get(white, Reserves),
    ExpectedBlack = maps:get(black, Reserves),
    ok.

%% Helper to count pieces on board
count_pieces_on_board(Pid) ->
    {ok, PiecesList} = binbo_bughouse:get_pieces_list(Pid, notation),
    erlang:length(PiecesList).

%% Helper to make a sequence of legal moves
make_moves(_Pid, []) ->
    ok;
make_moves(Pid, [Move | Tail]) ->
    case binbo_bughouse:move(Pid, Move) of
        {ok, continue} ->
            make_moves(Pid, Tail);
        {ok, _Status} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%------------------------------------------------------------------------------
%%%   Reserve Management Tests
%%%------------------------------------------------------------------------------

test_add_piece_to_reserve(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Initially empty reserves
    {ok, #{white := WhiteRes0, black := BlackRes0}} = binbo_bughouse:get_reserves(Pid),
    #{p := 0, n := 0, b := 0, r := 0, q := 0} = WhiteRes0,
    #{p := 0, n := 0, b := 0, r := 0, q := 0} = BlackRes0,

    %% Add pawn to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    {ok, #{white := WhiteRes1}} = binbo_bughouse:get_reserves(Pid),
    #{p := 1} = WhiteRes1,

    %% Add knight to black reserve
    ok = binbo_bughouse:add_to_reserve(Pid, black, n),
    {ok, #{black := BlackRes1}} = binbo_bughouse:get_reserves(Pid),
    #{n := 1} = BlackRes1,

    ok.

test_drop_decrements_reserve(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add pawn to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    {ok, #{white := #{p := 1}}} = binbo_bughouse:get_reserves(Pid),

    %% Drop pawn on e4
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>),

    %% Reserve should be empty now
    {ok, #{white := #{p := 0}}} = binbo_bughouse:get_reserves(Pid),

    ok.

test_add_multiple_pieces(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add 2 knights, 1 queen to white reserves
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),
    ok = binbo_bughouse:add_to_reserve(Pid, white, q),

    %% Verify counts
    {ok, #{white := WhiteRes}} = binbo_bughouse:get_reserves(Pid),
    #{n := 2, q := 1, p := 0, b := 0, r := 0} = WhiteRes,

    ok.

test_reserve_count_accuracy(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add various pieces
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, black, n),
    ok = binbo_bughouse:add_to_reserve(Pid, black, b),
    ok = binbo_bughouse:add_to_reserve(Pid, black, r),

    %% Verify exact counts
    {ok, Reserves} = binbo_bughouse:get_reserves(Pid),
    #{white := #{p := 3, n := 0, b := 0, r := 0, q := 0}} = Reserves,
    #{black := #{p := 0, n := 1, b := 1, r := 1, q := 0}} = Reserves,

    ok.

%%%------------------------------------------------------------------------------
%%%   Piece Drops Tests
%%%------------------------------------------------------------------------------

test_valid_piece_drop(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add pawn to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),

    %% Drop pawn on e4
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>),

    %% Verify pawn is on e4
    {ok, PiecesList} = binbo_bughouse:get_pieces_list(Pid, notation),
    true = lists:member({<<"e4">>, white, pawn}, PiecesList),

    %% Verify reserve decremented
    {ok, #{white := #{p := 0}}} = binbo_bughouse:get_reserves(Pid),

    ok.

test_drop_pawn_valid_squares(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add multiple pawns
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),

    %% Drop pawns on various valid squares
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"a7a6">>),  % Black moves
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@d3">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"b7b6">>),  % Black moves
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@f5">>),

    %% Verify all pawns placed
    {ok, #{white := #{p := 0}}} = binbo_bughouse:get_reserves(Pid),

    ok.

test_drop_knight(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add knight to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),

    %% Drop knight on d4
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"N@d4">>),

    %% Verify knight is on d4
    {ok, PiecesList} = binbo_bughouse:get_pieces_list(Pid, notation),
    true = lists:member({<<"d4">>, white, knight}, PiecesList),

    ok.

test_drop_bishop(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add bishop to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, b),

    %% Drop bishop on c3
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"B@c3">>),

    %% Verify bishop is on c3
    {ok, PiecesList} = binbo_bughouse:get_pieces_list(Pid, notation),
    true = lists:member({<<"c3">>, white, bishop}, PiecesList),

    ok.

test_drop_rook(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add rook to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, r),

    %% Drop rook on e5
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"R@e5">>),

    %% Verify rook is on e5
    {ok, PiecesList} = binbo_bughouse:get_pieces_list(Pid, notation),
    true = lists:member({<<"e5">>, white, rook}, PiecesList),

    ok.

test_drop_queen(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add queen to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, q),

    %% Drop queen on d5
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"Q@d5">>),

    %% Verify queen is on d5
    {ok, PiecesList} = binbo_bughouse:get_pieces_list(Pid, notation),
    true = lists:member({<<"d5">>, white, queen}, PiecesList),

    ok.

test_cannot_drop_pawn_on_rank_1(Config) ->
    Pid = get_pid(Config),
    %% Use board with pawns to avoid insufficient material
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Add pawn to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),

    %% Attempt to drop on rank 1 should fail
    {error, {{invalid_move, pawn_on_backrank}, <<"P@d1">>}} =
        binbo_bughouse:drop_move_uci(Pid, <<"P@d1">>),

    ok.

test_cannot_drop_pawn_on_rank_8(Config) ->
    Pid = get_pid(Config),
    %% Use board with pawns to avoid insufficient material
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Add pawn to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),

    %% Attempt to drop on rank 8 should fail
    {error, {{invalid_move, pawn_on_backrank}, <<"P@d8">>}} =
        binbo_bughouse:drop_move_uci(Pid, <<"P@d8">>),

    ok.

test_cannot_drop_on_occupied_square(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add pawn to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),

    %% Attempt to drop on e2 (occupied by white pawn) should fail
    {error, {{invalid_move, square_occupied}, <<"P@e2">>}} =
        binbo_bughouse:drop_move_uci(Pid, <<"P@e2">>),

    ok.

test_cannot_drop_without_reserve(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% No reserves - attempt to drop should fail
    {error, {{invalid_move, no_piece_in_reserve}, <<"P@e4">>}} =
        binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>),

    ok.

test_drop_notation_parsing(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add various pieces to reserves
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),
    ok = binbo_bughouse:add_to_reserve(Pid, white, b),
    ok = binbo_bughouse:add_to_reserve(Pid, white, r),
    ok = binbo_bughouse:add_to_reserve(Pid, white, q),

    %% Test parsing all piece types
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"a7a6">>),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"N@f3">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"b7b6">>),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"B@c4">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"c7c6">>),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"R@a3">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"d7d6">>),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"Q@d5">>),

    %% All reserves should be empty
    {ok, #{white := #{p := 0, n := 0, b := 0, r := 0, q := 0}}} =
        binbo_bughouse:get_reserves(Pid),

    ok.

%%%------------------------------------------------------------------------------
%%%   Legal Drops Tests
%%%------------------------------------------------------------------------------

test_all_legal_drops_empty_reserves(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% No reserves - should return empty list
    {ok, []} = binbo_bughouse:all_legal_drops(Pid),

    ok.

test_all_legal_drops_with_pawns(Config) ->
    Pid = get_pid(Config),
    %% Start with board with pawns to avoid insufficient material
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Add pawn to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),

    %% Get all legal drops
    {ok, Drops} = binbo_bughouse:all_legal_drops(Pid),

    %% Should have drops for all empty squares except ranks 1 and 8
    true = erlang:length(Drops) > 0,

    %% All drops should start with "P@"
    true = lists:all(fun(Drop) ->
        binary:part(Drop, 0, 2) =:= <<"P@">>
    end, Drops),

    %% Should not include rank 1 or rank 8
    false = lists:member(<<"P@e1">>, Drops),
    false = lists:member(<<"P@e8">>, Drops),

    ok.

test_all_legal_drops_with_knights(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Add knight to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),

    %% Get all legal drops
    {ok, Drops} = binbo_bughouse:all_legal_drops(Pid),

    %% Should have drops for all empty squares (knights can go on any rank)
    true = erlang:length(Drops) > 0,

    %% All drops should start with "N@"
    true = lists:all(fun(Drop) ->
        binary:part(Drop, 0, 2) =:= <<"N@">>
    end, Drops),

    ok.

test_all_legal_drops_multiple_pieces(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Add multiple piece types
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),
    ok = binbo_bughouse:add_to_reserve(Pid, white, q),

    %% Get all legal drops
    {ok, Drops} = binbo_bughouse:all_legal_drops(Pid),

    %% Should have drops for all three piece types
    HasPawnDrops = lists:any(fun(Drop) ->
        binary:part(Drop, 0, 2) =:= <<"P@">>
    end, Drops),
    HasKnightDrops = lists:any(fun(Drop) ->
        binary:part(Drop, 0, 2) =:= <<"N@">>
    end, Drops),
    HasQueenDrops = lists:any(fun(Drop) ->
        binary:part(Drop, 0, 2) =:= <<"Q@">>
    end, Drops),

    true = HasPawnDrops,
    true = HasKnightDrops,
    true = HasQueenDrops,

    ok.

test_can_drop_valid_square(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add pawn to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),

    %% e4 is empty, should be valid
    true = binbo_bughouse:can_drop(Pid, p, <<"e4">>),

    ok.

test_can_drop_invalid_square(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add pawn to white reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),

    %% e2 is occupied, should be invalid
    false = binbo_bughouse:can_drop(Pid, p, <<"e2">>),

    ok.

test_can_drop_no_piece_in_reserve(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% No reserves - cannot drop
    false = binbo_bughouse:can_drop(Pid, p, <<"e4">>),

    ok.

%%%------------------------------------------------------------------------------
%%%   Checkmate Tests
%%%------------------------------------------------------------------------------

test_checkmate_ends_game(Config) ->
    Pid = get_pid(Config),
    %% Smothered mate: black king h8, rook g8, pawns f7/g7/h7
    %% Knight check can't be blocked by drops
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"6rk/5ppp/8/4N3/8/8/8/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Deliver checkmate (Nf7#)
    {ok, {checkmate, white_wins}} = binbo_bughouse:move(Pid, <<"e5f7">>),

    %% Verify game status
    {ok, {checkmate, white_wins}} = binbo_bughouse:game_status(Pid),

    ok.

test_checkmate_returns_winner(Config) ->
    Pid = get_pid(Config),
    %% Scholar's mate position
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"rnbqkbnr/3ppppp/ppp5/8/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 1">>,
        #{mode => bughouse}),

    %% Deliver checkmate (Qxf7#)
    {ok, {checkmate, white_wins}} = binbo_bughouse:move(Pid, <<"f3f7">>),

    ok.

test_checkmate_white_wins(Config) ->
    Pid = get_pid(Config),
    %% Smothered mate: knight delivers mate on f7
    %% Black king h8, rook g8, pawns g7 h7; white knight on e5 goes to f7
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"6rk/5ppp/8/4N3/8/8/8/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Knight to f7 is checkmate (king trapped by own pieces)
    {ok, {checkmate, white_wins}} = binbo_bughouse:move(Pid, <<"e5f7">>),

    ok.

test_checkmate_black_wins(Config) ->
    Pid = get_pid(Config),
    %% Smothered mate by black: white king h1, rook g1, pawns g2/h2
    %% Black knight on e4 goes to f2 — knight check can't be blocked by drops
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/8/8/8/4n3/8/6PP/6RK b - - 0 1">>, #{mode => bughouse}),

    %% Black knight delivers mate (Nf2#)
    {ok, {checkmate, black_wins}} = binbo_bughouse:move(Pid, <<"e4f2">>),

    ok.

test_king_capture_illegal_in_bughouse_mode(Config) ->
    Pid = get_pid(Config),
    %% Position where queen can see king but capture is illegal
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/8/8/4Q3/8/8/8/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% King capture should fail in bughouse mode too
    {error, {{invalid_move, king_capture}, <<"e5e8">>}} =
        binbo_bughouse:move(Pid, <<"e5e8">>),

    ok.

%%%------------------------------------------------------------------------------
%%%   Bughouse Mode Validation Tests
%%%------------------------------------------------------------------------------

test_king_in_check_blocked_bughouse_mode(Config) ->
    Pid = get_pid(Config),
    %% White king in check from black rook on e8
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4r2k/8/8/8/8/8/8/R3K3 w - - 0 1">>, #{mode => bughouse}),

    %% Cannot move rook while king is in check (enforced in bughouse too now)
    {error, {{invalid_move, own_king_in_check}, <<"a1a2">>}} =
        binbo_bughouse:move(Pid, <<"a1a2">>),

    ok.

test_king_in_check_blocked_standard_mode(Config) ->
    Pid = get_pid(Config),
    %% White king in check from black rook on e8
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4r2k/8/8/8/8/8/8/R3K3 w - - 0 1">>, #{mode => standard}),

    %% Cannot move rook while king is in check in standard mode
    {error, {{invalid_move, own_king_in_check}, <<"a1a2">>}} =
        binbo_bughouse:move(Pid, <<"a1a2">>),

    ok.

test_move_into_check_blocked_bughouse_mode(Config) ->
    Pid = get_pid(Config),
    %% Black rook on d8 would attack d1
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"3rk3/8/8/8/8/8/8/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Cannot move king into check (enforced in bughouse too now)
    {error, {{invalid_move, own_king_in_check}, <<"e1d1">>}} =
        binbo_bughouse:move(Pid, <<"e1d1">>),

    ok.

test_move_into_check_standard_mode(Config) ->
    Pid = get_pid(Config),
    %% Black rook on d8 would attack d1
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"3rk3/8/8/8/8/8/8/4K3 w - - 0 1">>, #{mode => standard}),

    %% Cannot move king into check in standard mode
    {error, {{invalid_move, own_king_in_check}, <<"e1d1">>}} =
        binbo_bughouse:move(Pid, <<"e1d1">>),

    ok.

%%%------------------------------------------------------------------------------
%%%   FEN with Reserves Tests
%%%------------------------------------------------------------------------------

test_parse_fen_no_reserves(Config) ->
    Pid = get_pid(Config),
    %% Standard FEN with no reserves
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">>),

    %% Reserves should be empty
    {ok, #{white := WhiteRes, black := BlackRes}} = binbo_bughouse:get_reserves(Pid),
    #{p := 0, n := 0, b := 0, r := 0, q := 0} = WhiteRes,
    #{p := 0, n := 0, b := 0, r := 0, q := 0} = BlackRes,

    ok.

test_parse_fen_white_reserves_only(Config) ->
    Pid = get_pid(Config),
    %% FEN with white reserves only
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[NP] w KQkq - 0 1">>),

    %% Check reserves
    {ok, #{white := WhiteRes, black := BlackRes}} = binbo_bughouse:get_reserves(Pid),
    #{n := 1, p := 1, b := 0, r := 0, q := 0} = WhiteRes,
    #{p := 0, n := 0, b := 0, r := 0, q := 0} = BlackRes,

    ok.

test_parse_fen_black_reserves_only(Config) ->
    Pid = get_pid(Config),
    %% FEN with black reserves only
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[bp] w KQkq - 0 1">>),

    %% Check reserves
    {ok, #{white := WhiteRes, black := BlackRes}} = binbo_bughouse:get_reserves(Pid),
    #{p := 0, n := 0, b := 0, r := 0, q := 0} = WhiteRes,
    #{b := 1, p := 1, n := 0, r := 0, q := 0} = BlackRes,

    ok.

test_parse_fen_both_reserves(Config) ->
    Pid = get_pid(Config),
    %% FEN with both reserves
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[NNP][qr] w KQkq - 0 1">>),

    %% Check reserves
    {ok, #{white := WhiteRes, black := BlackRes}} = binbo_bughouse:get_reserves(Pid),
    #{n := 2, p := 1, b := 0, r := 0, q := 0} = WhiteRes,
    #{q := 1, r := 1, p := 0, n := 0, b := 0} = BlackRes,

    ok.

test_parse_fen_multiple_pieces(Config) ->
    Pid = get_pid(Config),
    %% FEN with multiple pieces in reserves (add pawns to avoid insufficient material)
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3[PPPNNBQ][ppnnbbrq] w - - 0 1">>),

    %% Check reserves
    {ok, #{white := WhiteRes, black := BlackRes}} = binbo_bughouse:get_reserves(Pid),
    #{p := 3, n := 2, b := 1, r := 0, q := 1} = WhiteRes,
    #{p := 2, n := 2, b := 2, r := 1, q := 1} = BlackRes,

    ok.

test_generate_fen_no_reserves(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial),

    %% Get FEN - should not include reserves
    {ok, Fen} = binbo_bughouse:get_fen(Pid),

    %% Should be standard initial FEN
    <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1">> = Fen,

    ok.

test_generate_fen_with_reserves(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add reserves
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),
    ok = binbo_bughouse:add_to_reserve(Pid, black, q),

    %% Get FEN - should include reserves
    {ok, Fen} = binbo_bughouse:get_fen(Pid),

    %% Should contain reserve notation
    true = binary:match(Fen, <<"[">>) =/= nomatch,
    true = binary:match(Fen, <<"]">>) =/= nomatch,

    ok.

test_fen_round_trip_with_reserves(Config) ->
    Pid = get_pid(Config),
    %% Load FEN with reserves in single-bracket BFEN format (add pawns to avoid insufficient material)
    Fen1 = <<"4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3[NPq] w - - 0 1">>,
    {ok, continue} = binbo_bughouse:new_game(Pid, Fen1, #{mode => bughouse}),

    %% Export FEN
    {ok, Fen2} = binbo_bughouse:get_fen(Pid),

    %% Should be byte-identical (single-bracket emit matches single-bracket input)
    Fen1 = Fen2,

    ok.

test_fen_round_trip_preserves_mode(Config) ->
    Pid = get_pid(Config),
    %% Start bughouse mode game with reserves
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),

    %% Export FEN
    {ok, Fen} = binbo_bughouse:get_fen(Pid),

    %% Create new game from FEN with bughouse mode
    {ok, Pid2} = binbo_bughouse:new_server(),
    {ok, continue} = binbo_bughouse:new_game(Pid2, Fen, #{mode => bughouse}),

    %% Verify mode preserved by checking drops work (bughouse-only feature)
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid2, <<"P@e4">>),

    binbo_bughouse:stop_server(Pid2),
    ok.

%%%------------------------------------------------------------------------------
%%%   Mode Persistence Tests
%%%------------------------------------------------------------------------------

test_default_mode_is_standard(Config) ->
    Pid = get_pid(Config),
    %% Default mode should be standard for backward compatibility
    {ok, continue} = binbo_bughouse:new_game(Pid),

    %% Test by trying to move into check (should fail in standard mode)
    %% Black rook on d8 attacks d1
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"3rk3/8/8/8/8/8/8/4K3 w - - 0 1">>),
    {error, {{invalid_move, own_king_in_check}, <<"e1d1">>}} =
        binbo_bughouse:move(Pid, <<"e1d1">>),

    ok.

test_bughouse_mode_initialization(Config) ->
    Pid = get_pid(Config),
    %% Initialize with bughouse mode
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Verify bughouse mode by testing drops work (bughouse-only feature)
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"N@e4">>),

    ok.

test_standard_mode_initialization(Config) ->
    Pid = get_pid(Config),
    %% Initialize with standard mode explicitly
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => standard}),

    %% Test by trying to move into check (should fail in standard mode)
    %% Black rook on d8 attacks d1
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"3rk3/8/8/8/8/8/8/4K3 w - - 0 1">>, #{mode => standard}),
    {error, {{invalid_move, own_king_in_check}, <<"e1d1">>}} =
        binbo_bughouse:move(Pid, <<"e1d1">>),

    ok.

test_mode_survives_fen_export_import(Config) ->
    Pid = get_pid(Config),
    %% Start in bughouse mode with reserves
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3 w - - 0 1">>, #{mode => bughouse}),
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),

    %% Export FEN
    {ok, Fen} = binbo_bughouse:get_fen(Pid),

    %% Import into new game with bughouse mode
    {ok, Pid2} = binbo_bughouse:new_server(),
    {ok, continue} = binbo_bughouse:new_game(Pid2, Fen, #{mode => bughouse}),

    %% Verify bughouse mode survived by testing drops work
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid2, <<"N@e4">>),

    binbo_bughouse:stop_server(Pid2),
    ok.

test_mode_in_game_state(Config) ->
    Pid = get_pid(Config),
    %% Initialize in bughouse mode
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Get game state
    Game = binbo_bughouse:game_state(Pid),
    true = erlang:is_map(Game),

    %% Mode should be in game state — verify via bughouse-specific behavior (drops)
    ok = binbo_bughouse:add_to_reserve(Pid, white, q),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"Q@d4">>),

    ok.

%%%------------------------------------------------------------------------------
%%%   Edge Cases Tests
%%%------------------------------------------------------------------------------

test_drop_after_game_over(Config) ->
    Pid = get_pid(Config),
    %% Smothered mate: knight check can't be blocked by drops
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"6rk/5ppp/8/4N3/8/8/8/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Add pawn to reserve
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),

    %% Deliver checkmate (Nf7#)
    {ok, {checkmate, white_wins}} = binbo_bughouse:move(Pid, <<"e5f7">>),

    %% Attempt to drop after game over
    {error, {{game_over, {checkmate, white_wins}}, <<"P@e4">>}} =
        binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>),

    ok.

test_drop_after_checkmate(Config) ->
    Pid = get_pid(Config),
    %% Smothered mate: knight check can't be blocked by drops
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"6rk/5ppp/8/4N3/8/8/8/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Deliver checkmate (Nf7#)
    {ok, {checkmate, white_wins}} = binbo_bughouse:move(Pid, <<"e5f7">>),

    %% Add pawn and try to drop
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    {error, {{game_over, {checkmate, white_wins}}, <<"P@e4">>}} =
        binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>),

    ok.

test_castling_in_bughouse_mode(Config) ->
    Pid = get_pid(Config),
    %% Position where white can castle
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1">>, #{mode => bughouse}),

    %% Castle kingside
    {ok, continue} = binbo_bughouse:move(Pid, <<"e1g1">>),

    %% Verify king and rook moved
    {ok, PiecesList} = binbo_bughouse:get_pieces_list(Pid, notation),
    true = lists:member({<<"g1">>, white, king}, PiecesList),
    true = lists:member({<<"f1">>, white, rook}, PiecesList),

    ok.

test_en_passant_in_bughouse_mode(Config) ->
    Pid = get_pid(Config),
    %% Set up en passant position
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Make moves to set up en passant
    ok = make_moves(Pid, [
        <<"e2e4">>, <<"a7a6">>,
        <<"e4e5">>, <<"d7d5">>
    ]),

    %% Capture en passant
    {ok, continue} = binbo_bughouse:move(Pid, <<"e5d6">>),

    ok.

test_promotion_in_bughouse_mode(Config) ->
    Pid = get_pid(Config),
    %% White has 9 pawns (valid in bughouse due to drops!)
    %% This tests that bughouse mode allows >8 pawns
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"7k/4P3/8/8/8/8/PPPPPPPP/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Promote to queen
    {ok, _Status} = binbo_bughouse:move(Pid, <<"e7e8q">>),

    %% Verify queen on e8
    {ok, PiecesList} = binbo_bughouse:get_pieces_list(Pid, notation),
    true = lists:member({<<"e8">>, white, queen}, PiecesList),

    ok.

test_multiple_drops_in_sequence(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add multiple pieces
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),
    ok = binbo_bughouse:add_to_reserve(Pid, white, b),

    %% Drop in sequence
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"a7a6">>),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"N@f3">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"b7b6">>),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"B@c4">>),

    %% Verify all pieces placed
    {ok, #{white := #{p := 0, n := 0, b := 0}}} = binbo_bughouse:get_reserves(Pid),

    ok.

test_drop_creates_check(Config) ->
    Pid = get_pid(Config),
    %% Position where dropping a piece creates check (add pawns to avoid insufficient material)
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3 w - - 0 1">>, #{mode => bughouse}),

    %% Add rook to reserves
    ok = binbo_bughouse:add_to_reserve(Pid, white, r),

    %% Drop rook to give check
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"R@e5">>),

    %% Move should succeed even though it gives check
    ok.

test_drop_creates_checkmate(Config) ->
    Pid = get_pid(Config),
    %% Black king a8 trapped by own rook b8 and pawns a7/b7.
    %% Drop N@c7: knight check can't be blocked, king has no escape.
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"kr6/pp6/8/8/8/8/8/4K3 w - - 0 1">>, #{mode => bughouse}),

    ok = binbo_bughouse:add_to_reserve(Pid, white, n),

    %% Drop knight on c7 for unblockable checkmate
    {ok, {checkmate, white_wins}} = binbo_bughouse:drop_move_uci(Pid, <<"N@c7">>),

    ok.

test_drop_blocks_check(Config) ->
    Pid = get_pid(Config),
    %% White king on e1 in check from black rook on e8.
    %% White has a rook in reserve. Dropping R@e4 blocks the check.
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4r1k1/8/8/8/8/8/8/4K3 w - - 0 1">>, #{mode => bughouse}),

    ok = binbo_bughouse:add_to_reserve(Pid, white, r),

    %% Drop rook on e4 to block the check along e-file
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"R@e4">>),

    ok.

test_drop_while_in_check_non_blocking(Config) ->
    Pid = get_pid(Config),
    %% White king on e1 in check from black rook on e8.
    %% White has a pawn in reserve. Dropping P@a3 does NOT block check.
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4r1k1/8/8/8/8/8/8/4K3 w - - 0 1">>, #{mode => bughouse}),

    ok = binbo_bughouse:add_to_reserve(Pid, white, p),

    %% Drop pawn on a3 — does not block check, should be rejected
    {error, {{invalid_move, own_king_in_check}, <<"P@a3">>}} =
        binbo_bughouse:drop_move_uci(Pid, <<"P@a3">>),

    ok.

test_empty_reserves_not_checkmate_bughouse(Config) ->
    Pid = get_pid(Config),
    %% Back-rank "mate": rook on a1 delivers check on a8.
    %% Black king g8, pawns f7/g7/h7. Empty squares b8-f8 could block.
    %% In bughouse, teammate can send pieces, so this is NOT checkmate.
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"6k1/5ppp/8/8/8/8/8/R3K3 w - - 0 1">>, #{mode => bughouse}),

    %% No reserves added! Ra1-a8 would be mate in standard chess,
    %% but in bughouse the game continues (teammate can send a blocking piece).
    {ok, continue} = binbo_bughouse:move(Pid, <<"a1a8">>),

    %% Verify game status is still continue, not checkmate
    {ok, continue} = binbo_bughouse:game_status(Pid),

    ok.

%%%------------------------------------------------------------------------------
%%%   Backward Compatibility Tests
%%%------------------------------------------------------------------------------

test_standard_mode_checkmate_works(Config) ->
    Pid = get_pid(Config),
    %% Scholar's mate position
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"rnbqkbnr/3ppppp/ppp5/8/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 1">>, #{mode => standard}),

    %% Deliver checkmate
    {ok, {checkmate, white_wins}} = binbo_bughouse:move(Pid, <<"f3f7">>),

    ok.

test_standard_mode_stalemate_works(Config) ->
    Pid = get_pid(Config),
    %% Stalemate position
    {ok, {draw, stalemate}} = binbo_bughouse:new_game(Pid,
        <<"7k/5Q2/6K1/8/8/8/8/8 b - - 0 1">>, #{mode => standard}),

    %% Verify stalemate status
    {ok, {draw, stalemate}} = binbo_bughouse:game_status(Pid),

    ok.

test_standard_mode_like_original_binbo(Config) ->
    Pid = get_pid(Config),
    %% Play a standard game
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => standard}),

    %% Make some standard moves
    ok = make_moves(Pid, [
        <<"e2e4">>, <<"e7e5">>,
        <<"g1f3">>, <<"b8c6">>,
        <<"f1c4">>, <<"f8c5">>
    ]),

    %% Should work just like standard chess
    {ok, continue} = binbo_bughouse:game_status(Pid),

    ok.

test_standard_mode_king_cannot_be_captured(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/8/8/4Q3/8/8/8/4K3 w - - 0 1">>, #{mode => standard}),

    %% Cannot capture king in standard mode
    {error, {{invalid_move, king_capture}, <<"e5e8">>}} =
        binbo_bughouse:move(Pid, <<"e5e8">>),

    %% Also verify king capture is illegal in bughouse mode
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/8/8/4Q3/8/8/8/4K3 w - - 0 1">>, #{mode => bughouse}),
    {error, {{invalid_move, king_capture}, <<"e5e8">>}} =
        binbo_bughouse:move(Pid, <<"e5e8">>),

    ok.

test_standard_mode_king_in_check_validation(Config) ->
    Pid = get_pid(Config),
    %% King is in check from rook on e8 (include black king)
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4r2k/8/8/8/8/8/8/R3K3 w - - 0 1">>, #{mode => standard}),

    %% Cannot move rook while king is in check in standard mode
    {error, {{invalid_move, own_king_in_check}, <<"a1a2">>}} =
        binbo_bughouse:move(Pid, <<"a1a2">>),

    ok.

test_standard_mode_rejects_too_many_pawns(Config) ->
    Pid = get_pid(Config),
    %% Try to load FEN with 9 white pawns in standard mode
    {error, {too_many_pawns, white, 9}} = binbo_bughouse:new_game(Pid,
        <<"7k/4P3/8/8/8/8/PPPPPPPP/4K3 w - - 0 1">>, #{mode => standard}),

    ok.

%%%------------------------------------------------------------------------------
%%%   BFEN Compliance Tests
%%%------------------------------------------------------------------------------

test_bfen_single_bracket_reserves(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, black, q),
    {ok, Fen} = binbo_bughouse:get_fen(Pid),
    %% Must contain [NPq] not [NP][q]
    true = binary:match(Fen, <<"[NPq]">>) =/= nomatch,
    nomatch = binary:match(Fen, <<"[NP][q]">>),
    ok.

test_bfen_empty_reserves_bughouse(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),
    {ok, Fen} = binbo_bughouse:get_fen(Pid),
    %% Bughouse mode with empty reserves must emit []
    <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[] w KQkq - 0 1">> = Fen,
    ok.

test_bfen_standard_mode_no_brackets(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => standard}),
    %% Manually add a pawn to reserve (normally impossible in standard but tests the emitter)
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    {ok, Fen} = binbo_bughouse:get_fen(Pid),
    %% Standard mode must not emit any brackets
    nomatch = binary:match(Fen, <<"[">>),
    ok.

test_bfen_canonical_reserve_ordering(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),
    %% Add white pieces in non-canonical order: P, Q, N
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, white, q),
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),
    {ok, Fen} = binbo_bughouse:get_fen(Pid),
    %% Output must be [QNP] (canonical: Q R B N P) regardless of insertion order
    true = binary:match(Fen, <<"[QNP]">>) =/= nomatch,
    ok.

test_bfen_promoted_piece_in_fen(Config) ->
    Pid = get_pid(Config),
    %% White pawn on e7, black rook on g8, black king on h8
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"6rk/4P3/8/8/8/8/8/4K3[] w - - 0 1">>, #{mode => bughouse}),
    %% Promote e7 pawn to queen
    {ok, _Status} = binbo_bughouse:move(Pid, <<"e7e8q">>),
    {ok, Fen} = binbo_bughouse:get_fen(Pid),
    %% Position must contain Q~ (promoted queen on e8)
    true = binary:match(Fen, <<"Q~">>) =/= nomatch,
    ok.

test_bfen_parse_fen_with_tilde(Config) ->
    Pid = get_pid(Config),
    %% Load FEN directly containing Q~ on e8; black rook on g8 can capture via clear f8
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4Q~1rk/8/8/8/8/8/8/4K3[] b - - 0 1">>, #{mode => bughouse}),
    %% get_capture_info for rook-captures-queen (g8 -> e8) must report was_promoted = true
    {ok, {q, true}} = binbo_bughouse:get_capture_info(Pid, <<"g8">>, <<"e8">>),
    %% Also verify FEN round-trips with Q~
    {ok, Fen} = binbo_bughouse:get_fen(Pid),
    true = binary:match(Fen, <<"Q~">>) =/= nomatch,
    ok.

test_bfen_promoted_round_trip(Config) ->
    Pid = get_pid(Config),
    %% Start from position where white can promote
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"6rk/4P3/8/8/8/8/8/4K3[] w - - 0 1">>, #{mode => bughouse}),
    {ok, _} = binbo_bughouse:move(Pid, <<"e7e8q">>),
    {ok, Fen} = binbo_bughouse:get_fen(Pid),
    %% Load the emitted FEN into a fresh game
    {ok, Pid2} = binbo_bughouse:new_server(),
    {ok, continue} = binbo_bughouse:new_game(Pid2, Fen, #{mode => bughouse}),
    %% The promoted queen on e8 must be flagged after round-trip — verify via capture_info
    {ok, {q, true}} = binbo_bughouse:get_capture_info(Pid2, <<"g8">>, <<"e8">>),
    binbo_bughouse:stop_server(Pid2),
    ok.

test_bfen_backward_compat_two_bracket_parse(Config) ->
    Pid = get_pid(Config),
    %% Old two-bracket format [NP][q] must still parse correctly
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3[NP][q] w - - 0 1">>, #{mode => bughouse}),
    {ok, #{white := #{n := 1, p := 1}, black := #{q := 1}}} =
        binbo_bughouse:get_reserves(Pid),
    ok.

%%%------------------------------------------------------------------------------
%%%   Integration Tests
%%%------------------------------------------------------------------------------

test_full_game_workflow(Config) ->
    Pid = get_pid(Config),
    %% Simulate two boards
    {ok, PidA} = binbo_bughouse:new_server(),
    {ok, PidB} = binbo_bughouse:new_server(),

    %% Start both games in bughouse mode
    {ok, continue} = binbo_bughouse:new_game(PidA, initial, #{mode => bughouse}),
    {ok, continue} = binbo_bughouse:new_game(PidB, initial, #{mode => bughouse}),

    %% Game A: Make some moves
    {ok, continue} = binbo_bughouse:move(PidA, <<"e2e4">>),
    {ok, continue} = binbo_bughouse:move(PidA, <<"e7e5">>),
    {ok, continue} = binbo_bughouse:move(PidA, <<"g1f3">>),
    {ok, continue} = binbo_bughouse:move(PidA, <<"b8c6">>),

    %% Game A: Capture pawn (Nxe5)
    {ok, continue} = binbo_bughouse:move(PidA, <<"f3e5">>),

    %% Add captured pawn to Game B's white reserves
    ok = binbo_bughouse:add_to_reserve(PidB, white, p),

    %% Game B: White can now drop the pawn
    {ok, continue} = binbo_bughouse:drop_move_uci(PidB, <<"P@e4">>),

    %% Verify reserves updated correctly
    {ok, #{white := #{p := 0}}} = binbo_bughouse:get_reserves(PidB),

    binbo_bughouse:stop_server(PidA),
    binbo_bughouse:stop_server(PidB),
    ok.

test_move_capture_add_drop_sequence(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Make capturing move
    {ok, continue} = binbo_bughouse:move(Pid, <<"e2e4">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"d7d5">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"e4d5">>),  % Capture pawn

    %% Add captured pawn to reserves (simulating partner board)
    ok = binbo_bughouse:add_to_reserve(Pid, black, p),
    {ok, #{black := #{p := 1}}} = binbo_bughouse:get_reserves(Pid),

    %% Black's turn, drop the pawn
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@e6">>),

    %% Verify reserve decreased and piece on board
    {ok, #{black := #{p := 0}}} = binbo_bughouse:get_reserves(Pid),
    {ok, PiecesList} = binbo_bughouse:get_pieces_list(Pid, notation),
    true = lists:member({<<"e6">>, black, pawn}, PiecesList),

    ok.

test_reserves_across_multiple_moves(Config) ->
    Pid = get_pid(Config),
    {ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}),

    %% Add multiple pieces to reserves
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, white, p),
    ok = binbo_bughouse:add_to_reserve(Pid, white, n),

    %% Drop pieces across multiple moves
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>),
    {ok, #{white := #{p := 1, n := 1}}} = binbo_bughouse:get_reserves(Pid),

    {ok, continue} = binbo_bughouse:move(Pid, <<"a7a6">>),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"N@f3">>),
    {ok, #{white := #{p := 1, n := 0}}} = binbo_bughouse:get_reserves(Pid),

    {ok, continue} = binbo_bughouse:move(Pid, <<"b7b6">>),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@d4">>),
    {ok, #{white := #{p := 0, n := 0}}} = binbo_bughouse:get_reserves(Pid),

    ok.

test_fen_load_with_reserves_and_play(Config) ->
    Pid = get_pid(Config),
    %% Load FEN with reserves (include some pieces to avoid insufficient material)
    {ok, continue} = binbo_bughouse:new_game(Pid,
        <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[NP][q] w KQkq - 0 1">>, #{mode => bughouse}),

    %% Verify reserves loaded
    {ok, #{white := #{n := 1, p := 1}, black := #{q := 1}}} =
        binbo_bughouse:get_reserves(Pid),

    %% Play using reserves
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"N@d4">>),
    {ok, continue} = binbo_bughouse:move(Pid, <<"a7a6">>),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>),
    {ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"Q@d5">>),

    %% All reserves should be empty
    {ok, #{white := #{n := 0, p := 0}, black := #{q := 0}}} =
        binbo_bughouse:get_reserves(Pid),

    ok.
