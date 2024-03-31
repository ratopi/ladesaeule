%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2023, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 01. Dez 2023 03:25
%%%-------------------------------------------------------------------
-module(cell_parser).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([start/2]).

-define(UNMASKED, unmasked).
-define(MASKED, masked).

-record(cell_parser_state, {callback, callback_state, cell_type = ?UNMASKED, current_cell = <<>>, current_line = []}).

%%%===================================================================
%%% API
%%%===================================================================

start(Callback, CallbackState) when is_function(Callback, 2) ->
	fun(Bin) ->
		remove_bom(Bin, #cell_parser_state{callback = Callback, callback_state = CallbackState})
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

remove_bom(<<16#ef, 16#bb, 16#bf, Bin/binary>>, State = #cell_parser_state{}) ->
	parse(Bin, State);

remove_bom(Bin, State = #cell_parser_state{}) ->
	parse(Bin, State).



parse(eof, _State = #cell_parser_state{callback = Callback, callback_state = CallbackState, current_cell = <<>>, current_line = []}) ->
	Callback(eof, CallbackState);

parse(eof, _State = #cell_parser_state{callback = Callback, callback_state = CallbackState, current_cell = Cell, current_line = Line}) ->
	io:fwrite("xxxx ~p~n", [_State]),
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

parse(<<$", _Rest/binary>>, _State = #cell_parser_state{cell_type = ?UNMASKED}) ->
	{error, {<<$">>, illegal_character}};

parse(<<Char, Rest/binary>>, State = #cell_parser_state{cell_type = ?MASKED, current_cell = Cell}) ->
	parse(Rest, State#cell_parser_state{current_cell = <<Cell/binary, Char>>});

% fill cell content

parse(<<Char, Rest/binary>>, State = #cell_parser_state{current_cell = <<>>}) ->
	parse(Rest, State#cell_parser_state{current_cell = <<Char>>, cell_type = ?UNMASKED});

parse(<<Char, Rest/binary>>, State = #cell_parser_state{current_cell = Cell}) ->
	parse(Rest, State#cell_parser_state{current_cell = <<Cell/binary, Char>>}).
