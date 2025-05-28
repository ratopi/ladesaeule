%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 28. Mär 2024 23:59
%%%-------------------------------------------------------------------
-module(lsl_data_converter).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([get_url/0, load_data/1]).

-record(starting, {io, infos = []}).
-record(got_header1, {io}).
-record(read_lines, {io, headers, first_line = true}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_url() ->
  UrlUrl = "https://raw.githubusercontent.com/ratopi/ladesaeule/refs/heads/master/url",
  case httpc:request(UrlUrl) of
    {ok, {{_, 200, _}, _, Url}} -> {ok, Url};
    {ok, {Head, _, _}} -> {error, {Head, UrlUrl}};
    Err -> {error, Err}
  end.


load_data(Url) ->
  {ok, IO} = file:open("./ladesaeulen.json", [binary, write]),

  % file:write(IO, <<${, 10>>),

  io:fwrite("loading ~p~n", [Url]),
  case httpc:request(get, {Url, []}, [], [{sync, false}, {stream, self}, {body_format, binary}]) of
    Err = {error, _} ->
      Err;
    {ok, RequestId} ->
      case parse_content(RequestId, cell_parser:start(fun handler_fun/2, #starting{io = IO})) of
        Err = {error, _} ->
          Err;
        {ok, eof, _} ->
          ok;
        X = {ok, Content, _Headers} ->
          io:fwrite("TODO ~p~n", [X]),
          todo
      end
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

log_fun(Fun) ->
  fun(A, B) ->
    io:fwrite("~p ~p -> ", [A, B]),
    R = Fun(A, B),
    io:fwrite("~p~n", [R]),
    R
  end.



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
  file:write(IO, <<${, 10, $[, 10>>),
  #read_lines{headers = Headers, io = IO};


handler_fun(eof, #read_lines{io = IO}) ->
  file:write(IO, <<10, $], 10, $}>>),
  file:close(IO),
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



parse_content(RequestId, CellParserFun) ->
  receive
    {http, {RequestId, stream_start, _Headers}} ->
      parse_content(RequestId, CellParserFun);

    {http, {RequestId, stream, BinBodyPart}} ->
      % io:fwrite("got ~p bytes~n", [size(BinBodyPart)]),
      parse_content(RequestId, CellParserFun(BinBodyPart));

    {http, {RequestId, stream_end, Headers}} ->
      {ok, CellParserFun(eof), Headers};

    {http, {RequestId, {{_, 404, _}, _Headers, _Content}}} ->
      {error, not_found}

  after 10000 ->
    {error, timeout}
  end.



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
        [4, 3, 2, 1]
      )
    end,
    build_map(Headers, Line, #{})
  ).



build_map([], [], Map) ->
  Map;

build_map([_ | Headers], [<<>> | Line], Map) ->
  build_map(Headers, Line, Map);

build_map([Fun | Headers], [V | Line], Map) ->
  MV = re:replace(V, <<"(", 194, 160, ")|(\n)$">>, <<>>, [{return, binary}]), % remove trailing space or newline in cell
  build_map(Headers, Line, Fun(MV, Map)).



build_headers(Headers) ->
  lists:map(fun set_fun/1, Headers).



set_fun(<<"Ladeeinrichtungs-ID">>) -> standard_fun(id);
set_fun(<<"Betreiber">>) -> standard_fun(operator);
set_fun(<<"Anzeigename (Karte)">>) -> standard_fun(display_name);
set_fun(<<"Status">>) -> standard_fun(status);
set_fun(<<"Art der Ladeeinrichtung">>) -> standard_fun(device_type);
set_fun(<<"Straße"/utf8>> = X) -> sub_map(addr, X);
set_fun(<<"Hausnummer">> = X) -> sub_map(addr, X);
set_fun(<<"Adresszusatz">> = X) -> sub_map(addr, X);
set_fun(<<"Postleitzahl">> = X) -> sub_map(addr, X);
set_fun(<<"Ort">> = X) -> sub_map(addr, X);
set_fun(<<"Bundesland">> = X) -> sub_map(addr, X);
set_fun(<<"Kreis/kreisfreie Stadt">> = X) -> sub_map(addr, X);
set_fun(<<"Breitengrad">>) -> to_float(sub_map(geo, lat));
set_fun(<<"Längengrad"/utf8>>) -> to_float(sub_map(geo, lon));
set_fun(<<"Standortbezeichnung">>) -> standard_fun(location_name);
set_fun(<<"Informationen zum Parkraum">>) -> standard_fun(parking_info);
set_fun(<<"Bezahlsysteme">>) -> standard_fun(payment);
set_fun(<<"Öffnungszeiten"/utf8>>) -> standard_fun(opening_hours);
set_fun(<<"Öffnungszeiten: Wochentage"/utf8>>) -> standard_fun(opening_weekdays);
set_fun(<<"Öffnungszeiten: Tageszeiten"/utf8>>) -> standard_fun(opening_daytime);
set_fun(<<"Inbetriebnahmedatum">> = X) -> sub_map(charging, X);
set_fun(<<"Nennleistung Ladeeinrichtung [kW]">> = X) -> sub_map(charging, X);
set_fun(<<"Art der Ladeeinrichung">> = X) -> sub_map(charging, X);
set_fun(<<"Anzahl Ladepunkte">>) -> ignore();
set_fun(<<"Steckertypen1">>) -> v_to_list(charing_point([charging, points, 1, plugs]));
set_fun(<<"Steckertypen2">>) -> v_to_list(charing_point([charging, points, 2, plugs]));
set_fun(<<"Steckertypen3">>) -> v_to_list(charing_point([charging, points, 3, plugs]));
set_fun(<<"Steckertypen4">>) -> v_to_list(charing_point([charging, points, 4, plugs]));
set_fun(<<"Steckertypen5">>) -> v_to_list(charing_point([charging, points, 5, plugs]));
set_fun(<<"Steckertypen6">>) -> v_to_list(charing_point([charging, points, 6, plugs]));
set_fun(<<"Nennleistung Stecker1">>) -> v_to_list(charing_point([charging, points, 1, power]));
set_fun(<<"Nennleistung Stecker2">>) -> v_to_list(charing_point([charging, points, 2, power]));
set_fun(<<"Nennleistung Stecker3">>) -> v_to_list(charing_point([charging, points, 3, power]));
set_fun(<<"Nennleistung Stecker4">>) -> v_to_list(charing_point([charging, points, 4, power]));
set_fun(<<"Nennleistung Stecker5">>) -> v_to_list(charing_point([charging, points, 5, power]));
set_fun(<<"Nennleistung Stecker6">>) -> v_to_list(charing_point([charging, points, 6, power]));
set_fun(<<"EVSE-ID1">>) -> v_to_list(charing_point([charging, points, 1, evse_id]));
set_fun(<<"EVSE-ID2">>) -> v_to_list(charing_point([charging, points, 2, evse_id]));
set_fun(<<"EVSE-ID3">>) -> v_to_list(charing_point([charging, points, 3, evse_id]));
set_fun(<<"EVSE-ID4">>) -> v_to_list(charing_point([charging, points, 4, evse_id]));
set_fun(<<"EVSE-ID5">>) -> v_to_list(charing_point([charging, points, 5, evse_id]));
set_fun(<<"EVSE-ID6">>) -> v_to_list(charing_point([charging, points, 6, evse_id]));
set_fun(<<"P1 [kW]">>) -> to_float(charing_point([charging, points, 1, kW]));
set_fun(<<"P2 [kW]">>) -> to_float(charing_point([charging, points, 2, kW]));
set_fun(<<"P3 [kW]">>) -> to_float(charing_point([charging, points, 3, kW]));
set_fun(<<"P4 [kW]">>) -> to_float(charing_point([charging, points, 4, kW]));
set_fun(<<"P5 [kW]">>) -> to_float(charing_point([charging, points, 5, kW]));
set_fun(<<"P6 [kW]">>) -> to_float(charing_point([charging, points, 6, kW]));
set_fun(<<"Public Key1">>) -> charing_point([charging, points, 1, pkey]);
set_fun(<<"Public Key2">>) -> charing_point([charging, points, 2, pkey]);
set_fun(<<"Public Key3">>) -> charing_point([charging, points, 3, pkey]);
set_fun(<<"Public Key4">>) -> charing_point([charging, points, 4, pkey]);
set_fun(<<"Public Key5">>) -> charing_point([charging, points, 5, pkey]);
set_fun(<<"Public Key6">>) -> charing_point([charging, points, 6, pkey]);
% unknown
set_fun(X) -> erlang:error({unknown_header, X}).



standard_fun(X) -> fun(V, Map) -> maps:put(X, V, Map) end.

sub_map(SubKey, Key) -> fun(V, Map) -> maps:put(SubKey, maps:put(Key, V, maps:get(SubKey, Map, #{})), Map) end.

charing_point(KeyPath) ->
  fun(V, Map) ->
    deep_put(KeyPath, V, Map)
  end.

ignore() -> fun(_, Map) -> Map end.


to_float(Fun) ->
  fun(V, Map) ->
    case string:split(V, <<$,>>) of
      [A, B] ->
        Fun(binary_to_float(<<A/binary, $., B/binary>>), Map);
      [_] ->
        Fun(binary_to_integer(V), Map)
    end
  end.



v_to_list(Fun) ->
  fun(V, Map) ->
    Fun(string:split(V, <<", ">>, all), Map)
  end.



deep_put([K], V, Map) ->
  maps:put(K, V, Map);

deep_put([K | T], V, Map) ->
  maps:put(K, deep_put(T, V, maps:get(K, Map, #{})), Map).



deep_change([K], F, Map) ->
  case maps:get(K, Map, undefined) of
    undefined -> Map;
    V -> maps:put(K, F(V), Map)
  end;

deep_change([K | T], F, Map) ->
  maps:put(K, deep_change(T, F, maps:get(K, Map, #{})), Map).
