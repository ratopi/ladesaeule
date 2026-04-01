%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2023, Ralf Thomas Pietsch
%%% @doc Continuation-based CSV parser for semicolon-separated,
%%% optionally quoted cells. Automatically detects UTF-8 (with BOM)
%%% and ISO-8859-1 encoded input. ISO-8859-1 is converted to UTF-8
%%% on the fly so that all output is always valid UTF-8.
%%% Returns a fun that accepts binary chunks or the atom `eof'.
%%% @end
%%% Created : 01. Dez 2023 03:25
%%%-------------------------------------------------------------------
-module(cell_parser).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([start/2]).

-define(UNMASKED, unmasked).
-define(MASKED, masked).

-define(ENC_UTF8, utf8).
-define(ENC_LATIN1, latin1).

-record(cell_parser_state, {callback, callback_state, cell_type = ?UNMASKED, encoding = ?ENC_UTF8, current_cell = <<>>, current_line = []}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a new parser function.
%% `Callback' is called with each parsed line (list of binaries) or the
%% atom `eof'. `CallbackState' is threaded through successive calls.
%% Returns a fun/1 that accepts a binary chunk or `eof'.
%% Automatically detects UTF-8 (with BOM) and ISO-8859-1 encoding.
%% ISO-8859-1 input is converted to UTF-8 on the fly.
-spec start(Callback, CallbackState) -> fun((binary() | eof) -> term()) when
    Callback :: fun(([binary()] | eof, State) -> State),
    CallbackState :: State,
    State :: term().
start(Callback, CallbackState) when is_function(Callback, 2) ->
	fun(Bin) ->
		detect_encoding(Bin, #cell_parser_state{callback = Callback, callback_state = CallbackState})
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Detects encoding from the first chunk.
%% UTF-8 BOM (EF BB BF) means UTF-8; otherwise we check whether
%% the data is valid UTF-8. If not, we assume ISO-8859-1.
detect_encoding(<<16#ef, 16#bb, 16#bf, Bin/binary>>, State = #cell_parser_state{}) ->
	parse(Bin, State#cell_parser_state{encoding = ?ENC_UTF8});

detect_encoding(Bin, State = #cell_parser_state{}) ->
	Encoding = guess_encoding(Bin),
	parse(Bin, State#cell_parser_state{encoding = Encoding}).

%% Guess whether the binary is valid UTF-8 or ISO-8859-1.
guess_encoding(Bin) ->
	case unicode:characters_to_binary(Bin, utf8) of
		{error, _, _} -> ?ENC_LATIN1;
		{incomplete, _, _} ->
			%% Could be a truncated multi-byte sequence at chunk boundary.
			%% Check the bulk: if most high bytes form valid UTF-8, assume UTF-8.
			case is_likely_utf8(Bin) of
				true -> ?ENC_UTF8;
				false -> ?ENC_LATIN1
			end;
		_ -> ?ENC_UTF8
	end.

%% Quick check: try to validate all but the last 3 bytes as UTF-8.
is_likely_utf8(Bin) when byte_size(Bin) > 3 ->
	Len = byte_size(Bin) - 3,
	<<Head:Len/binary, _/binary>> = Bin,
	case unicode:characters_to_binary(Head, utf8) of
		{error, _, _} -> false;
		_ -> true
	end;
is_likely_utf8(_) ->
	true.



parse(eof, _State = #cell_parser_state{callback = Callback, callback_state = CallbackState, current_cell = <<>>, current_line = []}) ->
	Callback(eof, CallbackState);

parse(eof, _State = #cell_parser_state{callback = Callback, callback_state = CallbackState, current_cell = Cell, current_line = Line}) ->
	NewCallbackState = Callback(lists:reverse([Cell | Line]), CallbackState),
	Callback(eof, NewCallbackState);

% separators and line breaks

parse(<<$;, Rest/binary>>, State = #cell_parser_state{cell_type = unmasked, current_cell = Cell, current_line = Line}) ->
	parse(Rest, State#cell_parser_state{current_cell = <<>>, current_line = [Cell | Line]});

parse(<<13, Rest/binary>>, State = #cell_parser_state{cell_type = ?UNMASKED}) ->
	parse(Rest, State#cell_parser_state{});

parse(<<10, Rest/binary>>, State = #cell_parser_state{callback = Callback, callback_state = CallbackState, current_cell = Cell, current_line = CurrentLine, cell_type = ?UNMASKED}) ->
	Line = lists:reverse([Cell | CurrentLine]),
	parse(Rest, State#cell_parser_state{callback_state = Callback(Line, CallbackState), current_cell = <<>>, current_line = []});

parse(<<>>, State = #cell_parser_state{}) ->
	fun(Bin) ->
		parse(Bin, State)
	end;

% string

parse(<<$", Rest/binary>>, State = #cell_parser_state{current_cell = <<>>}) ->
	parse(Rest, State#cell_parser_state{cell_type = ?MASKED});

parse(<<$", $", Rest/binary>>, State = #cell_parser_state{cell_type = ?MASKED, current_cell = Cell}) ->
	parse(Rest, State#cell_parser_state{current_cell = <<Cell/binary, $">>});

parse(<<$", Rest/binary>>, State = #cell_parser_state{cell_type = ?MASKED}) ->
	parse(Rest, State#cell_parser_state{cell_type = ?UNMASKED});
parse(<<$", Rest/binary>>, State = #cell_parser_state{cell_type = ?UNMASKED, current_cell = Cell}) ->
	parse(Rest, State#cell_parser_state{current_cell = <<Cell/binary, $">>});

parse(<<Char, Rest/binary>>, State = #cell_parser_state{cell_type = ?MASKED, current_cell = Cell, encoding = Enc}) ->
	Utf8Char = to_utf8_char(Char, Enc),
	parse(Rest, State#cell_parser_state{current_cell = <<Cell/binary, Utf8Char/binary>>});

% fill cell content

parse(<<Char, Rest/binary>>, State = #cell_parser_state{current_cell = <<>>, encoding = Enc}) ->
	Utf8Char = to_utf8_char(Char, Enc),
	parse(Rest, State#cell_parser_state{current_cell = Utf8Char, cell_type = ?UNMASKED});

parse(<<Char, Rest/binary>>, State = #cell_parser_state{current_cell = Cell, encoding = Enc}) ->
	Utf8Char = to_utf8_char(Char, Enc),
	parse(Rest, State#cell_parser_state{current_cell = <<Cell/binary, Utf8Char/binary>>}).


%% Convert a single byte to a UTF-8 binary.
%% For UTF-8 encoding, bytes are passed through as-is (they are already
%% part of valid UTF-8 multi-byte sequences handled byte by byte).
%% For Latin-1 encoding, bytes > 127 are converted to their UTF-8 representation.
to_utf8_char(Char, ?ENC_UTF8) ->
	<<Char>>;
to_utf8_char(Char, ?ENC_LATIN1) when Char < 128 ->
	<<Char>>;
to_utf8_char(Char, ?ENC_LATIN1) ->
	<<(16#C0 bor (Char bsr 6)), (16#80 bor (Char band 16#3F))>>.
