# Binbo-Bughouse

**Production-ready Bughouse chess engine based on binbo bitboards**

This is a fork of [DOBRO/binbo](https://github.com/DOBRO/binbo) extended to support Bughouse chess, a 2v2 chess variant where captured pieces can be placed back on your teammate's board.

## About This Fork

Binbo-Bughouse extends the excellent [binbo](https://github.com/DOBRO/binbo) chess library by Sergei Semichev to support Bughouse-specific features:

- ✅ **Piece drops**: Place captured pieces using `P@e4` notation (UCI standard)
- ✅ **Extended FEN**: Support for piece reserves `[NP]` format
- ✅ **Checkmate detection**: Standard chess legality enforced in bughouse mode
- ✅ **Configurable modes**: Switch between standard chess and Bughouse rules
- ✅ **Reserve management**: Track and manipulate piece reserves per board
- ✅ **Legal drop generation**: Calculate all valid piece placements
- ✅ **Dual board support**: Architecture for managing two simultaneous chess games with shared piece reserves

## Original Binbo Features (Inherited)

- ✅ Blazing fast move generation using Magic Bitboards
- ✅ Complete standard chess rules (en passant, castling, fifty-move rule, threefold repetition)
- ✅ Every game is an isolated Erlang process (gen_server)
- ✅ UCI protocol support for chess engines
- ✅ Cross-platform (Linux, Unix, Windows, macOS)
- ✅ Production-ready and battle-tested

## Requirements

- Erlang/OTP 20.0 or higher
- rebar3

## Installation

### For Erlang projects

Add to your `rebar.config`:

```erlang
{deps, [
  {binbo_bughouse, {git, "https://github.com/vcsawant/binbo-bughouse.git", {branch, "main"}}}
]}.
```

### For Elixir projects

Add to your `mix.exs`:

```elixir
defp deps do
  [
    {:binbo_bughouse, github: "vcsawant/binbo-bughouse", branch: "main"}
  ]
end
```

## Quick Start

### Erlang

```erlang
% Start application
{ok, _} = binbo_bughouse:start().

% Create a new Bughouse game
{ok, Pid} = binbo_bughouse:new_server().
{ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}).

% Make standard moves
{ok, continue} = binbo_bughouse:move(Pid, <<"e2e4">>).
{ok, continue} = binbo_bughouse:move(Pid, <<"e7e5">>).

% Add a piece to reserves (when partner captures a piece)
ok = binbo_bughouse:add_to_reserve(Pid, white, p).

% Get current reserves
{ok, #{white := WhiteReserve, black := BlackReserve}} = binbo_bughouse:get_reserves(Pid).

% Drop a piece from reserve
{ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>).

% Get all legal drops
{ok, Drops} = binbo_bughouse:all_legal_drops(Pid).
% => [<<"P@a3">>, <<"P@a4">>, <<"N@f3">>, ...]
```

### Elixir

```elixir
# Start application
{:ok, _} = :binbo_bughouse.start()

# Initialize two boards for Bughouse
{:ok, pid_a} = :binbo_bughouse.new_server()
{:ok, pid_b} = :binbo_bughouse.new_server()
{:ok, :continue} = :binbo_bughouse.new_game(pid_a, :initial, %{mode: :bughouse})
{:ok, :continue} = :binbo_bughouse.new_game(pid_b, :initial, %{mode: :bughouse})

# Make a move that captures a piece on board A
case :binbo_bughouse.move(pid_a, "dxe5") do
  {:ok, :continue} ->
    # Add captured piece to board B's reserves
    # (Elixir layer handles promoted piece demotion)
    :binbo_bughouse.add_to_reserve(pid_b, :white, :p)

  {:ok, {:checkmate, :white_wins}} ->
    # Game over! White won by checkmate on board A
    IO.puts("White wins by checkmate!")
end

# Drop a piece on board B
{:ok, :continue} = :binbo_bughouse.drop_move_uci(pid_b, "P@e4")

# Query legal drops
{:ok, drops} = :binbo_bughouse.all_legal_drops(pid_b)
# => ["P@a3", "P@a4", "P@b3", ...]

# Get reserves
{:ok, reserves} = :binbo_bughouse.get_reserves(pid_b)
# => %{white: %{p: 1, n: 0, b: 0, r: 0, q: 0}, black: %{...}}
```

## Bughouse Features

### Game Modes

Binbo-Bughouse supports both standard chess and Bughouse modes:

```erlang
% Standard chess mode (default - maintains backward compatibility)
{ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => standard}).

% Bughouse mode
{ok, continue} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}).
```

### Mode Differences

| Feature | Standard Mode | Bughouse Mode |
|---------|--------------|---------------|
| King in check validation | Required - move rejected if own king in check | Required - same as standard |
| King capture | Illegal move | Illegal move |
| Piece reserves | Tracked but not used | Active - pieces can be dropped |
| Piece drops block check | N/A | Drops can block check to prevent checkmate |
| Game termination | Checkmate/Stalemate | Checkmate/Stalemate |

### Reserve Management

```erlang
% Add a piece to reserves (called by Elixir when partner captures)
ok = binbo_bughouse:add_to_reserve(Pid, white, p).  % Add white pawn
ok = binbo_bughouse:add_to_reserve(Pid, black, n).  % Add black knight

% Get current reserves
{ok, #{white := WhiteRes, black := BlackRes}} = binbo_bughouse:get_reserves(Pid).
% WhiteRes = #{p => 2, n => 1, b => 0, r => 0, q => 0}
```

### Piece Drops

```erlang
% Drop using UCI notation (piece@square)
{ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>).
{ok, continue} = binbo_bughouse:drop_move_uci(Pid, <<"N@f3">>).

% Drop using piece type and square
{ok, continue} = binbo_bughouse:drop_move(Pid, p, <<"e4">>).
{ok, continue} = binbo_bughouse:drop_move(Pid, n, <<"f3">>).

% Check if a drop is legal
true = binbo_bughouse:can_drop(Pid, p, <<"e4">>).

% Get all legal drops for current position
{ok, Drops} = binbo_bughouse:all_legal_drops(Pid).
```

**Drop Restrictions:**
- Square must be empty
- Pawns cannot be dropped on ranks 1 or 8
- Piece must exist in current side's reserve

### Extended FEN Format

Standard chess FEN:
```
rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
```

Bughouse FEN (with reserves):
```
rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[NP] w KQkq - 0 1
                                               ^^^^
                                          piece reserves
```

### Piece Drop Notation

UCI standard `@` notation for drops:
```
P@e4    % Place pawn on e4
N@f3    % Place knight on f3
B@c4    % Place bishop on c4
```

## API Reference

### Game Initialization

```erlang
% Create a new game server
{ok, Pid} = binbo_bughouse:new_server().

% Start a game (defaults to standard mode for backward compatibility)
{ok, Status} = binbo_bughouse:new_game(Pid).
{ok, Status} = binbo_bughouse:new_game(Pid, initial).

% Start a game with specific mode
{ok, Status} = binbo_bughouse:new_game(Pid, initial, #{mode => bughouse}).
{ok, Status} = binbo_bughouse:new_game(Pid, FenString, #{mode => standard}).

% Load from extended FEN with reserves
{ok, Status} = binbo_bughouse:new_game(Pid, <<"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[NP] w KQkq - 0 1">>).
```

### Reserve Operations

```erlang
% Get reserves for both sides
{ok, #{white := WhiteReserve, black := BlackReserve}} = binbo_bughouse:get_reserves(Pid).

% Add piece to reserve (color: white | black, piece: p | n | b | r | q)
ok = binbo_bughouse:add_to_reserve(Pid, white, p).
ok = binbo_bughouse:add_to_reserve(Pid, black, n).
```

### Move Operations

```erlang
% Standard moves
{ok, Status} = binbo_bughouse:move(Pid, <<"e2e4">>).
{ok, Status} = binbo_bughouse:san_move(Pid, <<"e4">>).

% Piece drops
{ok, Status} = binbo_bughouse:drop_move_uci(Pid, <<"P@e4">>).
{ok, Status} = binbo_bughouse:drop_move(Pid, p, <<"e4">>).

% Get legal moves and drops
{ok, Moves} = binbo_bughouse:all_legal_moves(Pid, bin).
{ok, Drops} = binbo_bughouse:all_legal_drops(Pid).

% Check if specific drop is legal
true = binbo_bughouse:can_drop(Pid, p, <<"e4">>).
```

### Game Status

```erlang
% Possible status returns:
continue                                    % Game in progress
{checkmate, white_wins | black_wins}       % Checkmate (standard or bughouse mode)
{draw, Reason}                             % Draw (stalemate, rule50, etc.)
```

## Architecture for Elixir Integration

### Single Board Responsibility

Binbo-Bughouse manages **one board** at a time. The Elixir layer orchestrates:

**What binbo_bughouse DOES handle:**
- ✅ Single board chess rules
- ✅ Piece drop validation (square empty, not pawn on back rank)
- ✅ Move validation with checkmate detection
- ✅ Attack square calculation
- ✅ Extended FEN parsing with reserves `[NP]`
- ✅ Piece placement from reserves

**What binbo_bughouse does NOT handle (Elixir's job):**
- ❌ Two-board coordination
- ❌ Shared team reserves between boards
- ❌ Time management across boards
- ❌ Win condition propagation across both boards
- ❌ Move history synchronization
- ❌ Promoted piece tracking (for demotion on capture)

### Example: Two-Board Coordination in Elixir

```elixir
defmodule BughouseGame do
  # Track promoted pieces separately in Elixir
  defstruct [:board_a, :board_b, :promoted_pieces]

  def capture_piece(game, board, captured_piece, was_promoted) do
    # Determine piece type for reserve
    reserve_piece = if was_promoted, do: :p, else: captured_piece

    # Add to partner's board
    partner_board = if board == :a, do: game.board_b, else: game.board_a
    :binbo_bughouse.add_to_reserve(partner_board, color, reserve_piece)
  end

  def check_game_over(game) do
    # Check both boards for checkmate
    status_a = :binbo_bughouse.game_status(game.board_a)
    status_b = :binbo_bughouse.game_status(game.board_b)

    case {status_a, status_b} do
      {{:ok, {:checkmate, winner}}, _} -> {:game_over, winner}
      {_, {:ok, {:checkmate, winner}}} -> {:game_over, winner}
      _ -> :continue
    end
  end
end
```

## Development Status

**Status: Production Ready** ✅

All core Bughouse features are implemented and tested.

### Completed Features
- [x] Fork and rename project
- [x] Update application configuration
- [x] Preserve all original binbo functionality (93 tests passing)
- [x] Extended FEN parser for piece reserves `[NP]`
- [x] Piece drop move parsing and validation (`P@e4`)
- [x] Configurable game modes (standard vs bughouse)
- [x] Standard chess legality enforced (check validation, pins, checkmate)
- [x] Checkmate detection with drop-aware move generation
- [x] Reserve management API
- [x] Legal drop generation
- [x] Full backward compatibility

### Integration Status

This fork is ready for integration with the [Bughouse Chess web application](https://github.com/vcsawant/bughouse) built with Phoenix LiveView.

## Testing

```bash
# Run all tests
rebar3 ct

# Run specific test suite
rebar3 ct --suite test/binbo_fen_SUITE

# Compile and run tests
rebar3 compile && rebar3 ct
```

### Test Results

**All 93 tests passing** ✅

The implementation maintains full backward compatibility:
- ✅ All original binbo tests pass
- ✅ Standard chess mode works identically to original binbo
- ✅ Bughouse mode adds new functionality without breaking existing behavior
- ✅ FEN parsing handles both standard and extended formats
- ✅ Zero test failures

### Test Coverage

- **binbo_board_SUITE**: Board representation and square indexing
- **binbo_fen_SUITE**: FEN parsing (including extended format with reserves)
- **binbo_move_SUITE**: Move validation (both modes)
- **binbo_server_SUITE**: Gen_server functionality
- **perft_SUITE**: Move generation performance tests
- **play_game_SUITE**: Full game scenarios
- **pgn_SUITE**: PGN loading and parsing
- **uci_SUITE**: UCI protocol integration

## Attribution

**Original binbo** by Sergei Semichev (2019-2025)
- Repository: https://github.com/DOBRO/binbo
- License: Apache 2.0
- Documentation: https://hexdocs.pm/binbo

This fork maintains the same Apache 2.0 license and all original copyright notices.

## License

Apache License 2.0 - See [LICENSE](LICENSE) file for details.

Original work Copyright (c) 2019-2025 Sergei Semichev
Modified work Copyright (c) 2025 Viren Sawant

## Contributing

Contributions welcome! This fork focuses specifically on Bughouse chess support. For standard chess features, please contribute to the [original binbo project](https://github.com/DOBRO/binbo).

## Implementation Details

### Modified Modules

**Core Logic (9 files modified):**
- `src/binbo_position.erl` - Reserve management, mode-aware validation, checkmate detection
- `include/binbo_position.hrl` - New constants (GAME_KEY_WHITE_RESERVE, GAME_KEY_BLACK_RESERVE, GAME_KEY_MODE)
- `src/binbo_fen.erl` - Extended FEN parsing for `[NP]` reserve notation
- `include/binbo_fen.hrl` - Updated parsed_fen record with white_reserve and black_reserve
- `src/binbo_move.erl` - Drop move parsing and validation (`P@e4` notation)
- `src/binbo_movegen.erl` - Legal drop generation functions
- `src/binbo_game.erl` - High-level game API wrappers
- `src/binbo_server.erl` - Gen_server handlers for new operations
- `src/binbo_bughouse.erl` - Public API exports

### Data Structures

**Game State Extensions:**
```erlang
-type reserves() :: #{
    p => non_neg_integer(),  % pawns
    n => non_neg_integer(),  % knights
    b => non_neg_integer(),  % bishops
    r => non_neg_integer(),  % rooks
    q => non_neg_integer()   % queens
}.

-type game_mode() :: standard | bughouse.

% bb_game() now includes:
#{
    % ... existing fields ...
    white_reserve := reserves(),
    black_reserve := reserves(),
    mode := game_mode()
}
```

**Extended FEN Format:**
```
Position: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR
Reserves: [NNP][bp]  % White: 2 knights, 1 pawn; Black: 1 bishop, 1 pawn
Complete: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[NNP][bp] w KQkq - 0 1
```

### Key Design Decisions

1. **Backward Compatibility**: Defaults to standard mode, all existing tests pass
2. **Single Board Focus**: Each binbo_bughouse process manages one board
3. **Elixir Coordination**: Promoted piece tracking handled by Elixir layer
4. **Mode Configuration**: Explicit mode parameter for clarity and flexibility
5. **Checkmate Detection**: Drop-aware checkmate detection ensures drops that block check prevent false checkmates

## Performance

Inherits all performance characteristics from original binbo:
- **Magic Bitboards**: O(1) sliding piece move generation
- **Isolated Processes**: Each game runs independently
- **Zero-Copy Moves**: Efficient bitboard operations
- **Battle-Tested**: Production-ready codebase

Additional operations (piece drops, reserve queries) are O(1) map operations.

## Links

- **Original binbo**: https://github.com/DOBRO/binbo
- **Bughouse Chess Rules**: https://en.wikipedia.org/wiki/Bughouse_chess
- **UCI Protocol**: https://www.chessprogramming.org/UCI
- **Bughouse Web App**: https://github.com/vcsawant/bughouse
