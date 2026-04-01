%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024-2026, Ralf Thomas Pietsch
%%% @doc CSV to JSON conversion logic. Parses the BNetzA
%%% Ladesäulenregister CSV format and writes structured JSON.
%%% Delegates row-level conversion to lsl_row and all I/O
%%% (including JSON encoding) to lsl_output.
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_converter).

%% API
-export([new_parser/1, convert_file/2]).

-record(starting, {out :: lsl_output:lsl_output(), infos = []}).
-record(got_header1, {out :: lsl_output:lsl_output()}).
-record(read_lines, {out :: lsl_output:lsl_output(), row_state, first_line = true}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a new CSV parser that writes JSON via `Out'.
%% Returns a continuation function for feeding binary chunks.
-spec new_parser(lsl_output:lsl_output()) -> fun((binary() | eof) -> term()).
new_parser(Out) ->
  lsl_cell_parser:start(fun handler_fun/2, #starting{out = Out}).


%% @doc Converts a local CSV file to JSON. Used for testing.
-spec convert_file(string(), string()) -> {ok, term()} | {error, term()}.
convert_file(CsvPath, JsonPath) ->
  case file:read_file(CsvPath) of
    {ok, CsvBin} ->
      case lsl_output:open(JsonPath) of
        {ok, Out} ->
          lsl_output:write_begin(Out),
          Parser = new_parser(Out),
          Result = (Parser(CsvBin))(eof),
          lsl_output:write_meta(Out, CsvPath, undefined),
          lsl_output:write_end(Out),
          lsl_output:close(Out),
          {ok, Result};
        {error, Reason} ->
          {error, {open_file, Reason}}
      end;
    {error, Reason} ->
      {error, {read_file, Reason}}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% --- Handler callbacks for cell_parser ---

handler_fun([<<"Allgemeine Informationen">> | _], #starting{out = Out, infos = Infos}) ->
  lsl_output:write_json(Out, lsl_json:encode_field(<<"infos">>, lists:reverse(Infos))),
  lsl_output:write_data_separator(Out),
  lsl_output:write_json(Out, <<"\"data\":\n">>),
  #got_header1{out = Out};

handler_fun([<<>> | _], State = #starting{}) ->
  State;

handler_fun([Info | _], State = #starting{infos = Infos}) ->
  State#starting{infos = [Info | Infos]};

handler_fun(Headers, #got_header1{out = Out}) ->
  RowState = lsl_row:create(Headers),
  lsl_output:write_json(Out, <<$[, 10>>),
  #read_lines{row_state = RowState, out = Out};

handler_fun(eof, #read_lines{out = Out}) ->
  lsl_output:write_json(Out, <<10, $]>>),
  eof;

handler_fun([<<>> | _], State = #read_lines{}) ->
  State;

handler_fun(Line, State = #read_lines{out = Out, row_state = RowState, first_line = true}) ->
  Map = lsl_row:line_to_map(RowState, Line),
  lsl_output:write_json(Out, lsl_json:encode(Map)),
  State#read_lines{first_line = false};

handler_fun(Line, State = #read_lines{out = Out, row_state = RowState}) ->
  Map = lsl_row:line_to_map(RowState, Line),
  lsl_output:write_json(Out, <<$,, 10>>),
  lsl_output:write_json(Out, lsl_json:encode(Map)),
  State.
