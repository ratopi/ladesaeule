%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2026, Ralf Thomas Pietsch
%%% @doc Declarative mapping from BNetzA CSV column headers to JSON paths.
%%%
%%% Each mapping entry is a tuple:
%%%   {Type, CsvColumn, JsonPath}
%%%
%%% where:
%%%   Type      :: string         – value is copied as-is (binary)
%%%            | float           – comma-decimal converted to float
%%%            | list            – semicolon-separated value split into list
%%%            | list_float      – semicolon-separated, each element to float
%%%            | ignore          – column is skipped
%%%   CsvColumn :: binary()      – the CSV column header (2nd header row)
%%%   JsonPath  :: [atom()]      – the path in the output JSON map
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_mapping).

%% API
-export([mappings/0, build_fun/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns the declarative mapping table.
-spec mappings() -> [{Type :: atom(), CsvColumn :: binary(), JsonPath :: [atom()]}].
mappings() ->
  %% --- Allgemeine Informationen ---
  [
    {string, <<"Ladeeinrichtungs-ID">>,              [id]},
    {string, <<"Betreiber">>,                        [operator]},
    {string, <<"Anzeigename (Karte)">>,              [display_name]},
    {string, <<"Status">>,                           [status]},
    {string, <<"Art der Ladeeinrichtung">>,          [device_type]},
    {ignore, <<"Anzahl Ladepunkte">>,                []},
    {string, <<"Nennleistung Ladeeinrichtung [kW]">>, [charging, <<"Nennleistung Ladeeinrichtung [kW]">>]},
    {string, <<"Inbetriebnahmedatum">>,              [charging, <<"Inbetriebnahmedatum">>]},
    {string, <<"Art der Ladeeinrichung">>,           [charging, <<"Art der Ladeeinrichung">>]},
    {string, <<"Straße"/utf8>>,                      [addr, <<"Straße"/utf8>>]},
    {string, <<"Hausnummer">>,                       [addr, <<"Hausnummer">>]},
    {string, <<"Adresszusatz">>,                     [addr, <<"Adresszusatz">>]},
    {string, <<"Postleitzahl">>,                     [addr, <<"Postleitzahl">>]},
    {string, <<"Ort">>,                              [addr, <<"Ort">>]},
    {string, <<"Bundesland">>,                       [addr, <<"Bundesland">>]},
    {string, <<"Kreis/kreisfreie Stadt">>,           [addr, <<"Kreis/kreisfreie Stadt">>]},
    {float,  <<"Breitengrad">>,                      [geo, lat]},
    {float,  <<"Längengrad"/utf8>>,                  [geo, lon]},
    {string, <<"Standortbezeichnung">>,              [location_name]},
    {string, <<"Informationen zum Parkraum">>,       [parking_info]},
    {string, <<"Bezahlsysteme">>,                    [payment]},
    {string, <<"Öffnungszeiten"/utf8>>,              [opening_hours]},
    {string, <<"Öffnungszeiten: Wochentage"/utf8>>,  [opening_weekdays]},
    {string, <<"Öffnungszeiten: Tageszeiten"/utf8>>, [opening_daytime]}
  ] ++
  %% --- Ladepunkte 1–6 ---
  lists:append([charging_point_mappings(N) || N <- lists:seq(1, 6)]).


%%%===================================================================
%%% API – build setter function from a mapping entry
%%%===================================================================

%% @doc Builds a setter function `fun(Value, Map) -> Map' from a mapping entry.
-spec build_fun({atom(), binary(), [atom()]}) -> fun((term(), map()) -> map()).
build_fun({ignore, _, _}) ->
  fun(_, Map) -> Map end;

build_fun({string, _, Path}) ->
  fun(V, Map) -> deep_put(Path, V, Map) end;

build_fun({float, _, Path}) ->
  fun(V, Map) -> deep_put(Path, to_float(V), Map) end;

build_fun({list, _, Path}) ->
  fun(V, Map) -> deep_put(Path, split_list(V), Map) end;

build_fun({list_float, _, Path}) ->
  fun(V, Map) ->
    Vals = lists:map(fun to_float/1, split_list(V)),
    deep_put(Path, Vals, Map)
  end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% --- Ladepunkt-Mappings für einen gegebenen Index ---

charging_point_mappings(N) ->
  NB = integer_to_binary(N),
  [
    {list,   <<"Steckertypen", NB/binary>>,         [charging, points, N, plugs]},
    {list,   <<"Nennleistung Stecker", NB/binary>>, [charging, points, N, power]},
    {list,   <<"EVSE-ID", NB/binary>>,              [charging, points, N, evse_id]},
    {float,  <<"P", NB/binary, " [kW]">>,           [charging, points, N, kW]},
    {string, <<"Public Key", NB/binary>>,           [charging, points, N, pkey]}
  ].


%% --- Value conversion ---

to_float(V) when is_binary(V) ->
  case string:split(V, <<$,>>) of
    [A, B] -> binary_to_float(<<A/binary, $., B/binary>>);
    [_]    -> binary_to_integer(V)
  end;
to_float(V) ->
  V.

split_list(V) ->
  string:split(V, <<"; ">>, all).


%% --- Deep map put ---

deep_put([K], V, Map) ->
  maps:put(K, V, Map);
deep_put([K | T], V, Map) ->
  maps:put(K, deep_put(T, V, maps:get(K, Map, #{})), Map).
