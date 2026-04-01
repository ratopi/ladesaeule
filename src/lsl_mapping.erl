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
%%%            | integer         – parsed as integer
%%%            | float           – comma-decimal converted to float
%%%            | date            – DD.MM.YYYY converted to ISO 8601 (YYYY-MM-DD)
%%%            | list            – semicolon-separated value split into list of strings
%%%            | {list, Type}    – semicolon-separated, each element converted by Type
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
    {string,  <<"Ladeeinrichtungs-ID">>,               [id]},
    {string,  <<"Betreiber">>,                         [operator]},
    {string,  <<"Anzeigename (Karte)">>,               [display_name]},
    {string,  <<"Status">>,                            [status]},
    {string,  <<"Art der Ladeeinrichtung">>,           [device_type]},
    {ignore,  <<"Anzahl Ladepunkte">>,                 []},
    {float,   <<"Nennleistung Ladeeinrichtung [kW]">>, [charging, rated_power_kw]},
    {date,    <<"Inbetriebnahmedatum">>,               [charging, commissioning_date]},
    {string,  <<"Art der Ladeeinrichung">>,            [charging, device_type]},
    {string,  <<"Straße"/utf8>>,                       [addr, street]},
    {string,  <<"Hausnummer">>,                        [addr, house_number]},
    {string,  <<"Adresszusatz">>,                      [addr, address_extra]},
    {string,  <<"Postleitzahl">>,                      [addr, postcode]},
    {string,  <<"Ort">>,                               [addr, city]},
    {string,  <<"Bundesland">>,                        [addr, state]},
    {string,  <<"Kreis/kreisfreie Stadt">>,            [addr, district]},
    {float,   <<"Breitengrad">>,                       [geo, lat]},
    {float,   <<"Längengrad"/utf8>>,                   [geo, lon]},
    {string,  <<"Standortbezeichnung">>,               [location_name]},
    {string,  <<"Informationen zum Parkraum">>,        [access, parking]},
    {string,  <<"Bezahlsysteme">>,                     [access, payment]},
    {string,  <<"Öffnungszeiten"/utf8>>,               [access, 'opening_hours_raw']},
    {string,  <<"Öffnungszeiten: Wochentage"/utf8>>,   [access, 'opening_weekdays_raw']},
    {string,  <<"Öffnungszeiten: Tageszeiten"/utf8>>,  [access, 'opening_daytime_raw']}
  ] ++
  %% --- Ladepunkte 1–6 ---
  lists:append([charging_point_mappings(N) || N <- lists:seq(1, 6)]).


%% @doc Builds a setter function `fun(Value, Map) -> Map' from a mapping entry.
-spec build_fun({atom() | tuple(), binary(), [atom()]}) -> fun((term(), map()) -> map()).
build_fun({ignore, _, _}) ->
  fun(_, Map) -> Map end;
build_fun({string, _, Path}) ->
  fun(V, Map) -> deep_put(Path, V, Map) end;
build_fun({integer, _, Path}) ->
  fun(V, Map) -> deep_put(Path, to_integer(V), Map) end;
build_fun({float, _, Path}) ->
  fun(V, Map) -> deep_put(Path, to_float(V), Map) end;
build_fun({date, _, Path}) ->
  fun(V, Map) -> deep_put(Path, to_iso_date(V), Map) end;
build_fun({list, _, Path}) ->
  fun(V, Map) -> deep_put(Path, split_list(V), Map) end;
build_fun({{list, ElementType}, _, Path}) ->
  ElementFun = converter(ElementType),
  fun(V, Map) ->
    Vals = lists:map(ElementFun, split_list(V)),
    deep_put(Path, Vals, Map)
  end.


%% @doc Returns a list of post-processing functions to apply to each
%% assembled data row. Each function transforms `Map -> Map'.
-spec post_processors() -> [fun((map()) -> map())].
post_processors() ->
  [
    fun collect_charging_points/1,
    fun normalise_status/1,
    fun normalise_device_type/1,
    fun normalise_payment/1,
    fun normalise_parking/1,
    fun derive_opening_hours/1
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

%% Returns a fun/1 that converts a single value according to Type.
converter(string)  -> fun(V) -> V end;
converter(integer) -> fun to_integer/1;
converter(float)   -> fun to_float/1;
converter(date)    -> fun to_iso_date/1.

to_integer(V) when is_binary(V) ->
  binary_to_integer(V);
to_integer(V) ->
  V.

to_float(V) when is_binary(V) ->
  case string:split(V, <<$,>>) of
    [A, B] -> binary_to_float(<<A/binary, $., B/binary>>);
    [_]    -> binary_to_integer(V)
  end;
to_float(V) ->
  V.

%% Convert DD.MM.YYYY to YYYY-MM-DD (ISO 8601).
to_iso_date(<<D1, D2, $., M1, M2, $., Y1, Y2, Y3, Y4>>) ->
  <<Y1, Y2, Y3, Y4, $-, M1, M2, $-, D1, D2>>;
to_iso_date(V) ->
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

%% Translates BNetzA status values to OSM-style status tags.
normalise_status(Map) ->
  case maps:get(status, Map, undefined) of
    undefined -> Map;
    V -> maps:put(status, status_to_osm(V), Map)
  end.

%% Translates BNetzA device type to an English charging level tag.
normalise_device_type(Map) ->
  case maps:get(device_type, Map, undefined) of
    undefined -> Map;
    V -> maps:put(device_type, device_type_to_osm(V), Map)
  end.

%% Converts the semicolon-separated payment string into a list of
%% OSM-style payment tags.
normalise_payment(Map) ->
  Access = maps:get(access, Map, #{}),
  case maps:get(payment, Access, undefined) of
    undefined -> Map;
    V ->
      Tags = [payment_to_osm(string:trim(P))
              || P <- string:split(V, <<$;>>, all),
                 string:trim(P) =/= <<>>],
      maps:put(access, maps:put(payment, Tags, Access), Map)
  end.

%% Translates BNetzA parking restriction to OSM access tag.
normalise_parking(Map) ->
  Access = maps:get(access, Map, #{}),
  case maps:get(parking, Access, undefined) of
    undefined -> Map;
    V -> maps:put(access, maps:put(parking, parking_to_osm(V), Access), Map)
  end.

%% Derives an OSM `opening_hours' string from the three raw
%% opening-hours fields, writes it as `access.opening_hours',
%% and removes the raw helper fields.
derive_opening_hours(Map) ->
  Access0 = maps:get(access, Map, #{}),
  Hours    = maps:get('opening_hours_raw', Access0, <<>>),
  Weekdays = maps:get('opening_weekdays_raw', Access0, <<>>),
  Daytime  = maps:get('opening_daytime_raw', Access0, <<>>),
  Access1 = maps:without(['opening_hours_raw', 'opening_weekdays_raw', 'opening_daytime_raw'], Access0),
  case lsl_opening_hours:to_osm(Hours, Weekdays, Daytime) of
    {ok, OsmValue} ->
      maps:put(access, maps:put(opening_hours, OsmValue, Access1), Map);
    undefined ->
      maps:put(access, Access1, Map)
  end.

%% --- value translation tables ---

status_to_osm(<<"In Betrieb">>)     -> <<"operational">>;
status_to_osm(<<"In Planung">>)     -> <<"planned">>;
status_to_osm(<<"Außer Betrieb"/utf8>>)  -> <<"broken">>;
status_to_osm(<<"Stillgelegt">>)    -> <<"disused">>;
status_to_osm(<<"Im Bau">>)         -> <<"construction">>;
status_to_osm(Other)                -> Other.

device_type_to_osm(<<"Normalladeeinrichtung">>)  -> <<"normal">>;
device_type_to_osm(<<"Schnellladeeinrichtung">>) -> <<"rapid">>;
device_type_to_osm(Other)                        -> Other.

payment_to_osm(<<"Onlinezahlungsverfahren">>)        -> <<"app">>;
payment_to_osm(<<"RFID-Karte">>)                     -> <<"rfid">>;
payment_to_osm(<<"Kreditkarte (NFC)">>)              -> <<"contactless:credit_cards">>;
payment_to_osm(<<"Kreditkarte (Lesegerät)"/utf8>>)   -> <<"credit_cards">>;
payment_to_osm(<<"Debitkarte (NFC)">>)               -> <<"contactless:debit_cards">>;
payment_to_osm(<<"Debitkarte (Lesegerät)"/utf8>>)    -> <<"debit_cards">>;
payment_to_osm(<<"Plug & Charge">>)                  -> <<"plug_and_charge">>;
payment_to_osm(<<"Bargeld">>)                        -> <<"cash">>;
payment_to_osm(<<"Kostenlos">>)                      -> <<"free">>;
payment_to_osm(<<"Sonstige">>)                       -> <<"other">>;
payment_to_osm(Other)                                -> Other.

parking_to_osm(<<"Keine Beschränkung"/utf8>>)        -> <<"yes">>;
parking_to_osm(<<"keine Beschränkung"/utf8>>)        -> <<"yes">>;
parking_to_osm(<<"Nur für Kunden/Besucher"/utf8>>)   -> <<"customers">>;
parking_to_osm(<<"nur für Kunden/Besucher"/utf8>>)   -> <<"customers">>;
parking_to_osm(Other)                                -> Other.

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
