%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2026, Ralf Thomas Pietsch
%%% @doc Declarative, target-driven mapping from BNetzA CSV to JSON.
%%%
%%% The mapping describes the desired JSON structure. Each node
%%% specifies where its data comes from (which CSV column(s)) and
%%% how to convert it.
%%%
%%% Node types:
%%%
%%%   {Key, Type, CsvColumn}
%%%       Leaf node. Reads CsvColumn, converts with Type, stores as Key.
%%%       Type :: string | integer | float | date
%%%             | list             – split "; "-separated into string list
%%%             | {list, Type}     – split and convert each element
%%%             | {map, Fun}       – apply Fun(Value) for translation
%%%             | {split_map, Fun} – split on ";", trim, map each through Fun
%%%
%%%   {Key, Children}
%%%       Sub-object. Children is a list of nodes.
%%%
%%%   {Key, {'fun', Fun}, CsvColumns}
%%%       Custom function. Fun receives a map #{CsvCol => Value} and
%%%       returns the value for Key (or `undefined' to omit).
%%%       CsvColumns :: [binary()] – list of columns to pass.
%%%
%%%   {Key, {collect, From, To}, PointTemplate}
%%%       Collects numbered sub-objects From..To into a list.
%%%       PointTemplate is a fun(N) returning a list of child nodes
%%%       with column names parameterised by N.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_mapping).

%% API
-export([mapping/0, columns/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns the target-driven mapping tree.
-spec mapping() -> [tuple()].
mapping() ->
  [
    {id,            string,           <<"Ladeeinrichtungs-ID">>},
    {operator,      string,           <<"Betreiber">>},
    {display_name,  string,           <<"Anzeigename (Karte)">>},
    {status,        {map, fun status_to_osm/1},  <<"Status">>},
    {device_type,   {map, fun device_type_to_osm/1}, <<"Art der Ladeeinrichtung">>},
    {location_name, string,           <<"Standortbezeichnung">>},

    {addr, [
      {street,        string,  <<"Straße"/utf8>>},
      {house_number,  string,  <<"Hausnummer">>},
      {address_extra, string,  <<"Adresszusatz">>},
      {postcode,      string,  <<"Postleitzahl">>},
      {city,          string,  <<"Ort">>},
      {state,         string,  <<"Bundesland">>},
      {district,      string,  <<"Kreis/kreisfreie Stadt">>}
    ]},

    {geo, [
      {lat, float, <<"Breitengrad">>},
      {lon, float, <<"Längengrad"/utf8>>}
    ]},

    {charging, [
      {rated_power_kw,      float,  <<"Nennleistung Ladeeinrichtung [kW]">>},
      {commissioning_date,  date,   <<"Inbetriebnahmedatum">>},
      {device_type,         string, <<"Art der Ladeeinrichung">>},
      {points, {collect, 1, 6}, fun charging_point_template/1}
    ]},

    {access, [
      {parking,        {map, fun parking_to_osm/1},  <<"Informationen zum Parkraum">>},
      {payment,        {split_map, fun payment_to_osm/1}, <<"Bezahlsysteme">>},
      {opening_hours,  {'fun', fun opening_hours_fun/1},
                       [<<"Öffnungszeiten"/utf8>>,
                        <<"Öffnungszeiten: Wochentage"/utf8>>,
                        <<"Öffnungszeiten: Tageszeiten"/utf8>>]}
    ]}
  ].


%% @doc Returns a flat list of all CSV column names referenced by the mapping.
%% Used by lsl_row to build the column-index lookup.
-spec columns() -> [binary()].
columns() ->
  lists:usort(collect_columns(mapping())).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% --- charging point template ---

charging_point_template(N) ->
  NB = integer_to_binary(N),
  [
    {plugs,   list,   <<"Steckertypen", NB/binary>>},
    {power,   list,   <<"Nennleistung Stecker", NB/binary>>},
    {evse_id, list,   <<"EVSE-ID", NB/binary>>},
    {kW,      float,  <<"P", NB/binary, " [kW]">>},
    {pkey,    string, <<"Public Key", NB/binary>>}
  ].

%% --- opening hours custom function ---

opening_hours_fun(Cols) ->
  Hours    = maps:get(<<"Öffnungszeiten"/utf8>>, Cols, <<>>),
  Weekdays = maps:get(<<"Öffnungszeiten: Wochentage"/utf8>>, Cols, <<>>),
  Daytime  = maps:get(<<"Öffnungszeiten: Tageszeiten"/utf8>>, Cols, <<>>),
  case lsl_opening_hours:to_osm(Hours, Weekdays, Daytime) of
    {ok, V} -> V;
    undefined -> undefined
  end.

%% --- value translation tables ---

status_to_osm(<<"In Betrieb">>)          -> <<"operational">>;
status_to_osm(<<"In Planung">>)          -> <<"planned">>;
status_to_osm(<<"Außer Betrieb"/utf8>>)  -> <<"broken">>;
status_to_osm(<<"Stillgelegt">>)         -> <<"disused">>;
status_to_osm(<<"Im Bau">>)              -> <<"construction">>;
status_to_osm(Other)                     -> Other.

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

%% --- column collection from mapping tree ---

collect_columns([]) -> [];
collect_columns([Node | Rest]) ->
  collect_columns_node(Node) ++ collect_columns(Rest).

collect_columns_node({_Key, Children}) when is_list(Children) ->
  collect_columns(Children);
collect_columns_node({_Key, {collect, From, To}, TemplateFun}) ->
  lists:append([collect_columns(TemplateFun(N)) || N <- lists:seq(From, To)]);
collect_columns_node({_Key, {'fun', _Fun}, CsvCols}) when is_list(CsvCols) ->
  CsvCols;
collect_columns_node({_Key, _Type, CsvCol}) when is_binary(CsvCol) ->
  [CsvCol].
