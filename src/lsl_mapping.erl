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
%%% Post-processors are applied after all columns have been mapped.
%%% Each is a `fun(Map) -> Map' that transforms the assembled row.
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_mapping).

%% API
-export([mappings/0, build_fun/1, post_processors/0]).

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
    {string, <<"Informationen zum Parkraum">>,       [access, parking_info]},
    {string, <<"Bezahlsysteme">>,                    [access, payment]},
    {string, <<"Öffnungszeiten"/utf8>>,              [access, opening_hours]},
    {string, <<"Öffnungszeiten: Wochentage"/utf8>>,  [access, opening_weekdays]},
    {string, <<"Öffnungszeiten: Tageszeiten"/utf8>>, [access, opening_daytime]}
  ] ++
  %% --- Ladepunkte 1–6 ---
  lists:append([charging_point_mappings(N) || N <- lists:seq(1, 6)]).


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


%% @doc Returns a list of post-processing functions to apply to each
%% assembled data row. Each function transforms `Map -> Map'.
-spec post_processors() -> [fun((map()) -> map())].
post_processors() ->
  [
    fun collect_charging_points/1,
    fun derive_osm_opening_hours/1
  ].


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% --- charging point mappings ---

charging_point_mappings(N) ->
  NB = integer_to_binary(N),
  [
    {list,   <<"Steckertypen", NB/binary>>,         [charging, points, N, plugs]},
    {list,   <<"Nennleistung Stecker", NB/binary>>, [charging, points, N, power]},
    {list,   <<"EVSE-ID", NB/binary>>,              [charging, points, N, evse_id]},
    {float,  <<"P", NB/binary, " [kW]">>,           [charging, points, N, kW]},
    {string, <<"Public Key", NB/binary>>,           [charging, points, N, pkey]}
  ].

%% --- value conversion ---

to_float(V) when is_binary(V) ->
  case string:split(V, <<$,>>) of
    [A, B] -> binary_to_float(<<A/binary, $., B/binary>>);
    [_]    -> binary_to_integer(V)
  end;
to_float(V) ->
  V.

split_list(V) ->
  string:split(V, <<"; ">>, all).

%% --- post-processors ---

%% Converts the numbered charging point sub-maps (1–6) under
%% `charging.points' into a plain list, dropping empty slots.
collect_charging_points(Map) ->
  deep_change(
    [charging, points],
    fun(PointsMap) ->
      lists:foldl(
        fun(N, Acc) ->
          case maps:get(N, PointsMap, undefined) of
            undefined -> Acc;
            V -> [V | Acc]
          end
        end,
        [],
        [6, 5, 4, 3, 2, 1]
      )
    end,
    Map
  ).

%% Derives an OSM `opening_hours' string from the three BNetzA access
%% fields and stores it as `access.opening_hours_osm'.
derive_osm_opening_hours(Map) ->
  Access = maps:get(access, Map, #{}),
  Hours    = maps:get(opening_hours, Access, <<>>),
  Weekdays = maps:get(opening_weekdays, Access, <<>>),
  Daytime  = maps:get(opening_daytime, Access, <<>>),
  case lsl_opening_hours:to_osm(Hours, Weekdays, Daytime) of
    {ok, OsmValue} ->
      maps:put(access, maps:put(opening_hours_osm, OsmValue, Access), Map);
    undefined ->
      Map
  end.

%% --- deep map operations ---

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
