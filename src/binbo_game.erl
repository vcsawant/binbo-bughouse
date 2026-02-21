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

-module(binbo_game).

-export([new/1, new/2]).
-export([move/3, load_pgn/1, load_pgn_file/1]).
-export([status/1, draw/2, set_winner/3]).
-export([pretty_board/2, get_fen/1]).
-export([all_legal_moves/2]).
-export([select_square/2]).
-export([side_to_move/1]).
-export([get_pieces_list/2]).
%% Bughouse-specific exports
-export([get_reserves/1, add_to_reserve/3, get_capture_info/3]).
-export([drop_move/2, all_legal_drops/1, can_drop/3]).

%%%------------------------------------------------------------------------------
%%%   Includes
%%%------------------------------------------------------------------------------

-include("binbo_board.hrl").
-include("binbo_move.hrl").

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type parsed_fen() :: binbo_fen:parsed_fen().
-type bb_game() :: binbo_position:bb_game().
-type bb_game_error() :: binbo_position:bb_game_error().
-type game() :: undefined | bb_game().
-type game_fen() :: initial | binbo_fen:fen().
-type game_status() :: binbo_position:game_status().
-type sq_move() :: binbo_move:sq_move().
-type move_info() :: binbo_move:move_info().
-type filename() :: binary() | string().
-type legal_moves() :: [binbo_movegen:int_move()] | [binbo_movegen:bin_move()] | [binbo_movegen:str_move()].
-type bad_game_term() :: {bad_game, term()}.
-type init_error() :: binbo_fen:fen_error() | bb_game_error().
-type move_error() :: bad_game_term() | binbo_move:move_error().
-type gameover_status_error() :: {already_has_status, binbo_position:game_over_status()} | bad_game_term().
-type load_pgn_error() :: move_error() | binbo_pgn:pgn_error().
-type pretty_board_error() :: bad_game_term() | {bad_options, term()}.
-type status_ret() :: {ok, game_status()} | {error, bad_game_term()}.
-type get_fen_ret() :: {ok, binary()} | {error, bad_game_term()}.
-type all_legal_moves_ret() :: {ok, legal_moves()} | {error, bad_game_term()}.
-type side_to_move_ret() :: {ok, binbo_board:atom_color()} | {error, bad_game_term()}.
-type winner() :: term().

-export_type([game/0, game_fen/0, filename/0, winner/0]).
-export_type([game_status/0, status_ret/0, get_fen_ret/0]).
-export_type([all_legal_moves_ret/0, side_to_move_ret/0]).
-export_type([init_error/0, move_error/0, gameover_status_error/0]).
-export_type([load_pgn_error/0, pretty_board_error/0]).

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% new/1
-spec new(game_fen()) -> {ok, {bb_game(), game_status()}} | {error, init_error()}.
new(initial) ->
    new(binbo_fen:initial());
new(Fen) ->
    new(Fen, #{}).

%% new/2
-spec new(game_fen(), #{mode => standard | bughouse}) -> {ok, {bb_game(), game_status()}} | {error, init_error()}.
new(initial, Opts) ->
    new(binbo_fen:initial(), Opts);
new(Fen, Opts) ->
    case binbo_fen:parse(Fen) of
        {ok, ParsedFen} ->
            init_game(ParsedFen, Opts);
        Error ->
            Error
    end.

%% move/3
-spec move(sq | san | idx | int, sq_move(), game()) -> {ok, {bb_game(), game_status()}} | {error, move_error()}.
move(MoveNotation, Move, Game) when is_map(Game) ->
    Result = case MoveNotation of
        int -> binbo_move:validate_int_move(Move, Game);
        idx -> binbo_move:validate_idx_move(Move, Game);
        sq  -> binbo_move:validate_sq_move(Move, Game);
        san -> binbo_move:validate_san_move(Move, Game)
    end,
    case Result of
        {ok, MoveInfo, Game2} ->
            {ok, finalize_move(MoveInfo, Game2)};
        {error, Reason} ->
            {error, {Reason, Move}}
    end;
move(_MoveNotation, _Move, Game) ->
    {error, {bad_game, Game}}.


%% load_pgn/1
-spec load_pgn(binbo_pgn:pgn()) -> {ok, {bb_game(), game_status()}} | {error, load_pgn_error()}.
load_pgn(Pgn) ->
    case binbo_pgn:get_moves(Pgn) of
        {ok, Movelist} ->
            load_san_moves(Movelist);
        {error, _} = Error ->
            Error
    end.

%% load_pgn_file/1
-spec load_pgn_file(filename()) -> {ok, {bb_game(), game_status()}} | {error, any()}.
load_pgn_file(Filename) ->
    case uef_file:read_file_fast(Filename) of
        {ok, Pgn} ->
            load_pgn(Pgn);
        {error, _} = Error ->
            Error
    end.

%% pretty_board/2
-spec pretty_board(game(), binbo_position:pretty_board_opts()) -> {ok, {iolist(), binbo_position:game_status()}} | {error, pretty_board_error()}.
pretty_board(Game, Opts) when is_map(Game), is_list(Opts) ->
    {ok, binbo_position:pretty_board(Game, Opts)};
pretty_board(Game, Opts) when is_list(Opts) ->
    {error, {bad_game, Game}};
pretty_board(_, Opts) ->
    {error, {bad_options, Opts}}.

%% status/1
-spec status(game()) -> status_ret().
status(Game) ->
    case erlang:is_map(Game) of
        true  -> {ok, binbo_position:get_status(Game)};
        false -> {error, {bad_game, Game}}
    end.

%% draw/2
-spec draw(term(), game()) ->  {ok, bb_game()} | {error, gameover_status_error()}.
draw(Reason, Game) when is_map(Game) ->
    Status = binbo_position:get_status(Game),
    case binbo_position:is_status_inprogress(Status) of
        true  -> {ok, binbo_position:manual_draw(Reason, Game)};
        false -> {error, {already_has_status, Status}}
    end;
draw(_Reason, Game) ->
    {error, {bad_game, Game}}.

%% set_winner/3
-spec set_winner(game(), winner(), term()) ->  {ok, bb_game()} | {error, gameover_status_error()}.
set_winner(Game, Winner, Reason) when is_map(Game) ->
    Status = binbo_position:get_status(Game),
    case binbo_position:is_status_inprogress(Status) of
        true  -> {ok, binbo_position:set_manual_winner(Winner, Reason, Game)};
        false -> {error, {already_has_status, Status}}
    end;
set_winner(Game, _Winner, _Reason) ->
    {error, {bad_game, Game}}.

%% get_fen/1
-spec get_fen(game()) -> get_fen_ret().
get_fen(Game) ->
    case erlang:is_map(Game) of
        true  -> {ok, binbo_position:get_fen(Game)};
        false -> {error, {bad_game, Game}}
    end.

%% all_legal_moves/2
-spec all_legal_moves(game(), int | bin | str) -> all_legal_moves_ret().
all_legal_moves(Game, MoveType) ->
    case erlang:is_map(Game) of
        true  -> {ok, binbo_movegen:all_valid_moves(Game, MoveType)};
        false -> {error, {bad_game, Game}}
    end.

%% select_square/2
%% @doc Returns the piece at a square and all legal moves from that square.
%% Returns {ok, {PieceChar, LegalMoves}} where:
%%   - PieceChar is FEN character ($P, $p, $N, etc.) or 'empty'
%%   - LegalMoves is [{FromSquare, ToSquare}] or [{FromSquare, ToSquare, Promo}]
%% @end
-spec select_square(binary() | non_neg_integer(), game()) ->
    {ok, {byte() | empty, list()}} | {error, bad_game_term()}.
select_square(Square, Game) when is_map(Game) ->
    % Convert notation to index if needed
    SquareIdx = case Square of
        Bin when is_binary(Bin) -> binbo_board:notation_to_index(Bin);
        Idx when is_integer(Idx) -> Idx
    end,

    % Get piece at square
    Piece = binbo_position:get_piece(SquareIdx, Game),

    % Convert piece byte to FEN character
    PieceChar = case Piece of
        0 -> empty;  % EMPTY_SQUARE = 0
        _ -> ?PIECE_TO_CHAR(Piece)
    end,

    % Get all legal moves in binary format
    AllMoves = binbo_movegen:all_valid_moves(Game, bin),

    % Filter to moves starting from this square
    SquareNotation = binbo_board:index_to_notation(SquareIdx),
    LegalMoves = lists:filter(fun(Move) ->
        case Move of
            {From, _To} -> From =:= SquareNotation;
            {From, _To, _Promo} -> From =:= SquareNotation;
            _ -> false
        end
    end, AllMoves),

    {ok, {PieceChar, LegalMoves}};
select_square(_Square, Game) ->
    {error, {bad_game, Game}}.

%% side_to_move/1
-spec side_to_move(game()) -> side_to_move_ret().
side_to_move(Game) ->
    case erlang:is_map(Game) of
        true  -> {ok, binbo_position:plain_sidetomove(Game)};
        false -> {error, {bad_game, Game}}
    end.

%% get_pieces_list/1
-spec get_pieces_list(game(), index | notation) -> {ok, [binbo_position:sq_piece_tuple()]} | {error, bad_game_term()}.
get_pieces_list(Game, SquareType) ->
    case erlang:is_map(Game) of
        true  -> {ok, binbo_position:get_pieces_list(Game, SquareType)};
        false -> {error, {bad_game, Game}}
    end.


%%%------------------------------------------------------------------------------
%%%   API (Bughouse-specific)
%%%------------------------------------------------------------------------------

%% get_reserves/1
-spec get_reserves(game()) -> {ok, #{white => binbo_position:reserves(), black => binbo_position:reserves()}} | {error, bad_game_term()}.
get_reserves(Game) when is_map(Game) ->
    WhiteReserve = binbo_position:get_reserve(16#00, Game),  % ?WHITE
    BlackReserve = binbo_position:get_reserve(16#10, Game),  % ?BLACK
    {ok, #{white => WhiteReserve, black => BlackReserve}};
get_reserves(Game) ->
    {error, {bad_game, Game}}.

%% add_to_reserve/3
-spec add_to_reserve(white | black, p | n | b | r | q, game()) -> {ok, bb_game()} | {error, bad_game_term()}.
add_to_reserve(Color, PieceType, Game) when is_map(Game) ->
    ColorInt = case Color of white -> 16#00; black -> 16#10 end,  % ?WHITE, ?BLACK
    PieceTypeInt = case PieceType of
        p -> 16#01;  % ?PAWN
        n -> 16#02;  % ?KNIGHT
        b -> 16#03;  % ?BISHOP
        r -> 16#04;  % ?ROOK
        q -> 16#05   % ?QUEEN
    end,
    Game2 = binbo_position:add_piece_to_reserve(PieceTypeInt, ColorInt, Game),
    {ok, Game2};
add_to_reserve(_Color, _PieceType, Game) ->
    {error, {bad_game, Game}}.

%% get_capture_info/3
-spec get_capture_info(binary(), binary(), game()) -> {ok, {atom(), boolean()}} | {ok, no_capture} | {error, term()}.
get_capture_info(FromSquare, ToSquare, Game) when is_map(Game) ->
    % Convert square notation to indices
    FromIdx = binbo_board:notation_to_index(FromSquare),
    ToIdx = binbo_board:notation_to_index(ToSquare),
    binbo_position:get_capture_info(FromIdx, ToIdx, Game);
get_capture_info(_FromSquare, _ToSquare, Game) ->
    {error, {bad_game, Game}}.

%% drop_move/2
-spec drop_move(binary(), game()) -> {ok, {bb_game(), game_status()}} | {error, move_error()}.
drop_move(DropMove, Game) when is_map(Game) ->
    case binbo_move:validate_drop_move(DropMove, Game) of
        {ok, MoveInfo, Game2} ->
            {ok, finalize_move(MoveInfo, Game2)};
        {error, Reason} ->
            {error, {Reason, DropMove}}
    end;
drop_move(_DropMove, Game) ->
    {error, {bad_game, Game}}.

%% all_legal_drops/1
-spec all_legal_drops(game()) -> {ok, [binary()]} | {error, bad_game_term()}.
all_legal_drops(Game) when is_map(Game) ->
    Drops = binbo_movegen:all_legal_drops(Game),
    {ok, Drops};
all_legal_drops(Game) ->
    {error, {bad_game, Game}}.

%% can_drop/3
-spec can_drop(p | n | b | r | q, binary(), game()) -> boolean().
can_drop(PieceType, Square, Game) when is_map(Game) ->
    binbo_movegen:can_drop(PieceType, Square, Game);
can_drop(_PieceType, _Square, _Game) ->
    false.


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% init_game/2
-spec init_game(parsed_fen(), #{mode => standard | bughouse}) -> {ok, {bb_game(), game_status()}} | {error, bb_game_error()}.
init_game(ParsedFen, Opts) ->
    Game0 = binbo_position:init_bb_game(ParsedFen),
    % Apply mode from options if provided
    Game = case maps:get(mode, Opts, standard) of
        bughouse -> Game0#{mode => bughouse};
        standard -> Game0
    end,
    case binbo_position:validate_loaded_fen(Game) of
        ok ->
            {ok, finalize_fen(Game)};
        {error, _} = Error ->
            Error
    end.

%% finalize_fen/1
-spec finalize_fen(bb_game()) -> {bb_game(), game_status()}.
finalize_fen(Game0) ->
    SideToMove = binbo_position:get_sidetomove(Game0),
    HasValidMoves0 = binbo_movegen:has_valid_moves(SideToMove, Game0),
    IsCheck = binbo_position:is_in_check(SideToMove, Game0),
    %% In bughouse mode, also consider drops for initial status
    HasValidMoves = case HasValidMoves0 of
        true -> true;
        false ->
            case binbo_position:get_mode(Game0) of
                bughouse ->
                    binbo_movegen:has_any_valid_drop(SideToMove, IsCheck, Game0);
                standard ->
                    false
            end
    end,
    Game = binbo_position:with_status(Game0, HasValidMoves, IsCheck),
    Status = binbo_position:get_status(Game),
    {Game, Status}.

%% finalize_move/2
-spec finalize_move(move_info(), bb_game()) -> {bb_game(), game_status()}.
finalize_move(MoveInfo, Game0) ->
    EnemyColor = binbo_move:enemy_color(MoveInfo),
    HasValidMoves0 = binbo_movegen:has_valid_moves(EnemyColor, Game0),
    IsCheck = binbo_position:is_in_check(EnemyColor, Game0),
    %% In bughouse mode, also consider drops when determining if opponent has valid moves.
    %% A player in check might be able to drop a piece to block the check.
    HasValidMoves = case HasValidMoves0 of
        true -> true;
        false ->
            case binbo_position:get_mode(Game0) of
                bughouse ->
                    binbo_movegen:has_any_valid_drop(EnemyColor, IsCheck, Game0);
                standard ->
                    false
            end
    end,
    MoveInfo2 = MoveInfo#move_info{
        is_check = IsCheck,
        has_valid_moves = HasValidMoves
    },
    Game = binbo_position:finalize_move(MoveInfo2, Game0),
    Status = binbo_position:get_status(Game),
    {Game, Status}.


%% load_san_moves
-spec load_san_moves([sq_move()]) -> {ok, {bb_game(), game_status()}} | {error, move_error()}.
load_san_moves(Movelist) ->
    load_moves(Movelist, san).

%% load_moves/2
-spec load_moves([sq_move()], san | sq) -> {ok, {bb_game(), game_status()}} | {error, move_error()}.
load_moves(Movelist, MoveNotation) ->
    {ok, {Game, Status}} = new(initial),
    load_moves(Movelist, MoveNotation, Game, Status).

%% load_moves/4
-spec load_moves([sq_move()], san | sq, bb_game(), game_status()) -> {ok, {bb_game(), game_status()}} | {error, move_error()}.
load_moves([], _MoveNotation, Game, Status) ->
    {ok, {Game, Status}};
load_moves([Move|Tail], MoveNotation, Game0, _Status0) ->
    case move(MoveNotation, Move, Game0) of
        {ok, {Game, Status}} ->
            load_moves(Tail, MoveNotation, Game, Status);
        {error, _} = Error ->
            Error
    end.
