%%%-------------------------------------------------------------------
%%% @doc Tests for cell_parser, especially edge cases around quoting.
%%%-------------------------------------------------------------------
-module(cell_parser_tests).

-include_lib("eunit/include/eunit.hrl").

%% --- Helper ---

%% Parses a binary CSV snippet and collects all lines into a list.
%% Returns the list of parsed lines (each line is a list of binaries).
parse_collect(Bin) ->
	Callback = fun
		(eof, Acc) -> lists:reverse(Acc);
		(Line, Acc) -> [Line | Acc]
	end,
	Parser = lsl_cell_parser:start(Callback, []),
	(Parser(Bin))(eof).


%% --- Tests ---

simple_cells_test() ->
	Result = parse_collect(<<"a;b;c\n">>),
	?assertEqual([[<<"a">>, <<"b">>, <<"c">>]], Result).

empty_cells_test() ->
	Result = parse_collect(<<";;;\n">>),
	?assertEqual([[<<>>, <<>>, <<>>, <<>>]], Result).

quoted_cell_test() ->
	Result = parse_collect(<<"\"hello;world\";b\n">>),
	?assertEqual([[<<"hello;world">>, <<"b">>]], Result).

quoted_cell_with_escaped_quote_test() ->
	Result = parse_collect(<<"\"he said \"\"hi\"\"\";b\n">>),
	?assertEqual([[<<"he said \"hi\"">>, <<"b">>]], Result).

%% This is the critical test for the bug fix:
%% A double-quote appearing inside an unquoted cell must be
%% treated as a literal character, not cause a parse error.
unquoted_cell_with_quote_test() ->
	Result = parse_collect(<<"abc\"def;ghi\n">>),
	?assertEqual([[<<"abc\"def">>, <<"ghi">>]], Result).

unquoted_cell_with_multiple_quotes_test() ->
	Result = parse_collect(<<"a\"b\"c;d\n">>),
	?assertEqual([[<<"a\"b\"c">>, <<"d">>]], Result).

unquoted_cell_quote_at_end_test() ->
	Result = parse_collect(<<"abc\";def\n">>),
	?assertEqual([[<<"abc\"">>, <<"def">>]], Result).

%% Quote after closing quote in a masked cell (edge case):
%% ;"foo"bar; -> the part after the closing quote is unmasked content
quoted_then_unquoted_text_test() ->
	Result = parse_collect(<<"\"foo\"bar;baz\n">>),
	?assertEqual([[<<"foobar">>, <<"baz">>]], Result).

multiple_lines_test() ->
	Result = parse_collect(<<"a;b\nc;d\n">>),
	?assertEqual([[<<"a">>, <<"b">>], [<<"c">>, <<"d">>]], Result).

%% Chunked input: binary split across two calls
chunked_input_test() ->
	Callback = fun
		(eof, Acc) -> lists:reverse(Acc);
		(Line, Acc) -> [Line | Acc]
	end,
	Parser = lsl_cell_parser:start(Callback, []),
	Parser2 = Parser(<<"abc;d">>),
	Parser3 = Parser2(<<"ef\n">>),
	Result = Parser3(eof),
	?assertEqual([[<<"abc">>, <<"def">>]], Result).

%% Chunked input where quote is split across chunks
chunked_quote_test() ->
	Callback = fun
		(eof, Acc) -> lists:reverse(Acc);
		(Line, Acc) -> [Line | Acc]
	end,
	Parser = lsl_cell_parser:start(Callback, []),
	Parser2 = Parser(<<"abc">>),
	Parser3 = Parser2(<<"\"def;ghi\n">>),
	Result = Parser3(eof),
	?assertEqual([[<<"abc\"def">>, <<"ghi">>]], Result).

