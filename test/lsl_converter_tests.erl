%%%-------------------------------------------------------------------
%%% @doc Tests for lsl_converter using sample CSV files.
%%% Tests run against both a UTF-8 and an ISO-8859-1 encoded file.
%%%-------------------------------------------------------------------
-module(lsl_converter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SAMPLE_CSV_UTF8, "test/data/ladesaeulen_sample.csv").
-define(SAMPLE_CSV_ISO, "test/data/ladesaeulen_sample.2.csv").
-define(OUTPUT_JSON_UTF8, "test/data/test_output_utf8.json").
-define(OUTPUT_JSON_ISO, "test/data/test_output_iso.json").

%% --- Helper ---

convert_sample(CsvPath, JsonPath) ->
  {ok, _} = lsl_converter:convert_file(CsvPath, JsonPath),
  {ok, Bin} = file:read_file(JsonPath),
  jsx:decode(Bin, [return_maps]).

%% --- Test generator: runs the same tests for both encodings ---

utf8_test_() ->
  Map = convert_sample(?SAMPLE_CSV_UTF8, ?OUTPUT_JSON_UTF8),
  common_tests("UTF-8", Map, 19) ++
  utf8_specific_tests(Map) ++
  [{<<"UTF-8 cleanup">>, fun() -> file:delete(?OUTPUT_JSON_UTF8) end}].

iso_test_() ->
  Map = convert_sample(?SAMPLE_CSV_ISO, ?OUTPUT_JSON_ISO),
  common_tests("ISO-8859-1", Map, 9) ++
  [{<<"ISO-8859-1 cleanup">>, fun() -> file:delete(?OUTPUT_JSON_ISO) end}].

%% --- Common tests for both encodings ---

common_tests(Label, Map, ExpectedRows) ->
  #{<<"data">> := Data} = Map,
  [First | _] = Data,
  #{<<"addr">> := Addr} = First,
  #{<<"geo">> := Geo} = First,
  #{<<"charging">> := Charging} = First,
  #{<<"points">> := Points} = Charging,
  [P1 | _] = Points,
  [
    {iolist_to_binary([Label, " produces valid JSON"]), fun() ->
      ?assert(is_map(Map)),
      ?assertMatch(#{<<"infos">> := _, <<"data">> := _, <<"meta">> := _}, Map)
    end},
    {iolist_to_binary([Label, " infos contain header lines"]), fun() ->
      #{<<"infos">> := Infos} = Map,
      ?assert(is_list(Infos)),
      ?assert(length(Infos) > 0),
      [InfoFirst | _] = Infos,
      ?assertNotEqual(nomatch, binary:match(InfoFirst, <<"Ladesäulenregister"/utf8>>))
    end},
    {iolist_to_binary([Label, " data has expected entry count"]), fun() ->
      ?assertEqual(ExpectedRows, length(Data))
    end},
    {iolist_to_binary([Label, " meta contains source"]), fun() ->
      #{<<"meta">> := Meta} = Map,
      ?assertMatch(#{<<"source">> := _, <<"download_time">> := _}, Meta)
    end},
    {iolist_to_binary([Label, " first entry has all general fields"]), fun() ->
      ?assertMatch(#{<<"id">> := _}, First),
      ?assertMatch(#{<<"operator">> := _}, First),
      ?assertMatch(#{<<"status">> := _}, First),
      ?assertMatch(#{<<"device_type">> := _}, First),
      ?assertMatch(#{<<"addr">> := _}, First),
      ?assertMatch(#{<<"geo">> := _}, First),
      ?assertMatch(#{<<"charging">> := _}, First)
    end},
    {iolist_to_binary([Label, " first entry address fields (english keys)"]), fun() ->
      ?assertMatch(#{<<"street">> := _}, Addr),
      ?assertMatch(#{<<"house_number">> := _}, Addr),
      ?assertMatch(#{<<"postcode">> := _}, Addr),
      ?assertMatch(#{<<"city">> := _}, Addr),
      ?assertMatch(#{<<"state">> := _}, Addr),
      ?assertMatch(#{<<"district">> := _}, Addr)
    end},
    {iolist_to_binary([Label, " first entry geo is numeric"]), fun() ->
      #{<<"lat">> := Lat, <<"lon">> := Lon} = Geo,
      ?assert(is_number(Lat)),
      ?assert(is_number(Lon))
    end},
    {iolist_to_binary([Label, " first entry charging points"]), fun() ->
      ?assertMatch(#{<<"points">> := _}, Charging),
      ?assert(is_list(Points)),
      ?assertEqual(2, length(Points))
    end},
    {iolist_to_binary([Label, " charging point has plugs"]), fun() ->
      ?assertMatch(#{<<"plugs">> := _}, P1),
      #{<<"plugs">> := Plugs} = P1,
      ?assert(is_list(Plugs)),
      ?assert(length(Plugs) > 0)
    end},
    {iolist_to_binary([Label, " charging point has power"]), fun() ->
      ?assertMatch(#{<<"power">> := _}, P1)
    end},
    {iolist_to_binary([Label, " charging point has evse_id"]), fun() ->
      ?assertMatch(#{<<"evse_id">> := _}, P1)
    end},
    {iolist_to_binary([Label, " charging point has pkey"]), fun() ->
      ?assertMatch(#{<<"pkey">> := _}, P1)
    end},
    {iolist_to_binary([Label, " entry with four points"]), fun() ->
      Second = lists:nth(2, Data),
      #{<<"charging">> := #{<<"points">> := Pts}} = Second,
      ?assertEqual(4, length(Pts))
    end},
    {iolist_to_binary([Label, " rapid charger has multiple plug types"]), fun() ->
      Entry = lists:nth(6, Data),
      ?assertEqual(<<"rapid">>, maps:get(<<"device_type">>, Entry)),
      #{<<"charging">> := #{<<"points">> := Pts}} = Entry,
      ?assertEqual(2, length(Pts)),
      [_P1E, P2E] = Pts,
      #{<<"plugs">> := Plugs2} = P2E,
      ?assertEqual(2, length(Plugs2))
    end},
    {iolist_to_binary([Label, " status is OSM value"]), fun() ->
      ?assertEqual(<<"operational">>, maps:get(<<"status">>, First))
    end},
    {iolist_to_binary([Label, " device_type is OSM value"]), fun() ->
      ?assertEqual(<<"normal">>, maps:get(<<"device_type">>, First))
    end},
    {iolist_to_binary([Label, " rated_power_kw is number"]), fun() ->
      ?assert(is_number(maps:get(<<"rated_power_kw">>, Charging)))
    end},
    {iolist_to_binary([Label, " commissioning_date is ISO 8601"]), fun() ->
      Date = maps:get(<<"commissioning_date">>, Charging),
      %% "11.01.2020" → "2020-01-11"
      ?assertEqual(<<"2020-01-11">>, Date)
    end},
    {iolist_to_binary([Label, " access fields"]), fun() ->
      ?assertMatch(#{<<"access">> := _}, First),
      #{<<"access">> := Access} = First,
      ?assertMatch(#{<<"opening_hours">> := _}, Access),
      ?assertMatch(#{<<"payment">> := _}, Access),
      ?assertMatch(#{<<"parking">> := _}, Access),
      %% raw fields must be gone
      ?assertEqual(undefined, maps:get(<<"opening_hours_raw">>, Access, undefined)),
      ?assertEqual(undefined, maps:get(<<"opening_weekdays_raw">>, Access, undefined)),
      ?assertEqual(undefined, maps:get(<<"opening_daytime_raw">>, Access, undefined))
    end},
    {iolist_to_binary([Label, " opening_hours is 24/7"]), fun() ->
      #{<<"access">> := Access247} = First,
      ?assertEqual(<<"24/7">>, maps:get(<<"opening_hours">>, Access247))
    end},
    {iolist_to_binary([Label, " opening_hours absent when unknown"]), fun() ->
      Second = lists:nth(2, Data),
      Access2 = maps:get(<<"access">>, Second, #{}),
      ?assertEqual(undefined, maps:get(<<"opening_hours">>, Access2, undefined))
    end},
    {iolist_to_binary([Label, " payment is list of OSM tags"]), fun() ->
      #{<<"access">> := AccessP} = First,
      #{<<"payment">> := Payment} = AccessP,
      ?assert(is_list(Payment)),
      ?assert(lists:member(<<"rfid">>, Payment)),
      ?assert(lists:member(<<"app">>, Payment))
    end},
    {iolist_to_binary([Label, " parking is OSM access tag"]), fun() ->
      #{<<"access">> := AccessPark} = First,
      ?assertEqual(<<"yes">>, maps:get(<<"parking">>, AccessPark))
    end},
    {iolist_to_binary([Label, " display name"]), fun() ->
      ?assertMatch(#{<<"display_name">> := _}, First)
    end},
    {iolist_to_binary([Label, " address_extra"]), fun() ->
      Entry7 = lists:nth(7, Data),
      #{<<"addr">> := Addr7} = Entry7,
      ?assertMatch(#{<<"address_extra">> := <<"Parkplatz Albhotel">>}, Addr7),
      Entry8 = lists:nth(8, Data),
      #{<<"addr">> := Addr8} = Entry8,
      ?assertMatch(#{<<"address_extra">> := <<"Parkplatz Albhotel Bahnhoefle">>}, Addr8)
    end},
    {iolist_to_binary([Label, " location name"]), fun() ->
      Entry = lists:nth(7, Data),
      ?assertMatch(#{<<"location_name">> := _}, Entry)
    end}
  ].

%% --- UTF-8 specific tests (rows only present in the larger file) ---

utf8_specific_tests(Map) ->
  #{<<"data">> := Data} = Map,
  [
    {<<"UTF-8 has 19 entries for extended checks">>, fun() ->
      ?assertEqual(19, length(Data))
    end}
  ].
