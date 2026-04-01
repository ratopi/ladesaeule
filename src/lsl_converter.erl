%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024-2026, Ralf Thomas Pietsch
%%% @doc CSV to JSON conversion logic. Parses the BNetzA
%%% Ladesäulenregister CSV format and writes structured JSON.
%%% Uses lsl_mapping for the structure definition and lsl_row
%%% for row-level conversion.
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_converter).

%% API
-export([new_parser/1, convert_file/2]).

-record(starting, {io, infos = []}).
-record(got_header1, {io}).
-record(read_lines, {io, row_state, first_line = true}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a new CSV parser that writes JSON to `IO'.
%% Returns a continuation function for feeding binary chunks.
-spec new_parser(file:io_device()) -> fun((binary() | eof) -> term()).
new_parser(IO) ->
  lsl_cell_parser:start(fun handler_fun/2, #starting{io = IO}).


%% @doc Converts a local CSV file to JSON. Used for testing.
-spec convert_file(string(), string()) -> {ok, term()} | {error, term()}.
convert_file(CsvPath, JsonPath) ->
  case file:read_file(CsvPath) of
    {ok, CsvBin} ->
      case file:open(JsonPath, [binary, write]) of
        {ok, IO} ->
          lsl_json:write_begin(IO),
          Parser = new_parser(IO),
          Result = (Parser(CsvBin))(eof),
          lsl_json:write_meta(IO, CsvPath, undefined),
          lsl_json:write_end(IO),
          lsl_json:close(IO),
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

handler_fun([<<"Allgemeine Informationen">> | _], #starting{io = IO, infos = Infos}) ->
  file:write(IO, <<"\"infos\":", 10>>),
  file:write(IO, jsx:encode(lists:reverse(Infos))),
  file:write(IO, <<$,, 10, "\"data\":", 10>>),
  #got_header1{io = IO};

handler_fun([<<>> | _], State = #starting{}) ->
  State;

handler_fun([Info | _], State = #starting{infos = Infos}) ->
  State#starting{infos = [Info | Infos]};

handler_fun(Headers, #got_header1{io = IO}) ->
  RowState = lsl_row:create(Headers),
  file:write(IO, <<$[, 10>>),
  #read_lines{row_state = RowState, io = IO};

handler_fun(eof, #read_lines{io = IO}) ->
  file:write(IO, <<10, $]>>),
  eof;

handler_fun([<<>> | _], State = #read_lines{}) ->
  State;

handler_fun(Line, State = #read_lines{io = IO, row_state = RowState, first_line = true}) ->
  Map = lsl_row:line_to_map(RowState, Line),
  file:write(IO, jsx:encode(Map)),
  State#read_lines{first_line = false};

handler_fun(Line, State = #read_lines{io = IO, row_state = RowState}) ->
  Map = lsl_row:line_to_map(RowState, Line),
  file:write(IO, <<$,, 10>>),
  file:write(IO, jsx:encode(Map)),
  State.
