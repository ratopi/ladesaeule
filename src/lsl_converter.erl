%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024-2026, Ralf Thomas Pietsch
%%% @doc CSV to JSON conversion logic. Parses the BNetzA
%%% Ladesäulenregister CSV format and writes structured JSON.
%%% Uses lsl_mapping for the declarative column→JSON-path mapping.
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_converter).

%% API
-export([new_parser/1, convert_file/2]).

-record(starting, {io, infos = []}).
-record(got_header1, {io}).
-record(read_lines, {io, headers, first_line = true}).

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

handler_fun(Headers2, #got_header1{io = IO}) ->
  Headers = build_headers(Headers2),
  file:write(IO, <<$[, 10>>),
  #read_lines{headers = Headers, io = IO};

handler_fun(eof, #read_lines{io = _IO}) ->
  file:write(_IO, <<10, $]>>),
  eof;

handler_fun([<<>> | _], State = #read_lines{}) ->
  State;

handler_fun(Line, State = #read_lines{io = IO, headers = Headers, first_line = true}) ->
  Map = build_map(Headers, Line),
  file:write(IO, jsx:encode(Map)),
  State#read_lines{first_line = false};

handler_fun(Line, State = #read_lines{io = IO, headers = Headers}) ->
  Map = build_map(Headers, Line),
  file:write(IO, <<$,, 10>>),
  file:write(IO, jsx:encode(Map)),
  State.


%% --- Map building ---

build_map(Headers, Line) ->
  deep_change(
    [charging, points],
    fun(Map) ->
      lists:foldl(
        fun(N, L) ->
          case maps:get(N, Map, undefined) of
            undefined -> L;
            V -> [V | L]
          end
        end,
        [],
        [6, 5, 4, 3, 2, 1]
      )
    end,
    build_map(Headers, Line, #{})
  ).

build_map([], [], Map) ->
  Map;

build_map([_ | Headers], [<<>> | Line], Map) ->
  build_map(Headers, Line, Map);

build_map([Fun | Headers], [V | Line], Map) ->
  MV = re:replace(V, <<"(", 194, 160, ")|(\n)$">>, <<>>, [{return, binary}]),
  build_map(Headers, Line, Fun(MV, Map)).


%% --- Header mapping via lsl_mapping ---

build_headers(Headers) ->
  MappingTable = lsl_mapping:mappings(),
  MappingMap = maps:from_list([{Col, Entry} || Entry = {_, Col, _} <- MappingTable]),
  lists:map(
    fun(ColName) ->
      case maps:get(ColName, MappingMap, undefined) of
        undefined -> erlang:error({unknown_header, ColName});
        Entry -> lsl_mapping:build_fun(Entry)
      end
    end,
    Headers
  ).


%% --- Deep map operations ---

deep_change([K], F, Map) ->
  case maps:get(K, Map, undefined) of
    undefined -> Map;
    V -> maps:put(K, F(V), Map)
  end;

deep_change([K | T], F, Map) ->
  maps:put(K, deep_change(T, F, maps:get(K, Map, #{})), Map).
