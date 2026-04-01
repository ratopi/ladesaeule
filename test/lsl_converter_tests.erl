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
    {iolist_to_binary([Label, " first entry address fields"]), fun() ->
      ?assertMatch(#{<<"Straße"/utf8>> := _}, Addr),
      ?assertMatch(#{<<"Hausnummer">> := _}, Addr),
      ?assertMatch(#{<<"Postleitzahl">> := _}, Addr),
      ?assertMatch(#{<<"Ort">> := _}, Addr),
      ?assertMatch(#{<<"Bundesland">> := _}, Addr),
      ?assertMatch(#{<<"Kreis/kreisfreie Stadt">> := _}, Addr)
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
    {iolist_to_binary([Label, " Schnellladeeinrichtung has multiple plug types"]), fun() ->
      Entry = lists:nth(6, Data),
      ?assertEqual(<<"Schnellladeeinrichtung">>, maps:get(<<"device_type">>, Entry)),
      #{<<"charging">> := #{<<"points">> := Pts}} = Entry,
      ?assertEqual(2, length(Pts)),
      [_P1E, P2E] = Pts,
      #{<<"plugs">> := Plugs2} = P2E,
      ?assertEqual(2, length(Plugs2))
    end},
    {iolist_to_binary([Label, " opening hours fields"]), fun() ->
      ?assertMatch(#{<<"opening_hours">> := _}, First),
      ?assertMatch(#{<<"opening_weekdays">> := _}, First),
      ?assertMatch(#{<<"opening_daytime">> := _}, First)
    end},
    {iolist_to_binary([Label, " display name"]), fun() ->
      ?assertMatch(#{<<"display_name">> := _}, First)
    end},
    {iolist_to_binary([Label, " payment info"]), fun() ->
      ?assertMatch(#{<<"payment">> := _}, First)
    end},
    {iolist_to_binary([Label, " Adresszusatz"]), fun() ->
      Entry7 = lists:nth(7, Data),
      #{<<"addr">> := Addr7} = Entry7,
      ?assertMatch(#{<<"Adresszusatz">> := <<"Parkplatz Albhotel">>}, Addr7),
      Entry8 = lists:nth(8, Data),
      #{<<"addr">> := Addr8} = Entry8,
      ?assertMatch(#{<<"Adresszusatz">> := <<"Parkplatz Albhotel Bahnhoefle">>}, Addr8)
    end},
    {iolist_to_binary([Label, " location name"]), fun() ->
      Entry = lists:nth(7, Data),
      ?assertMatch(#{<<"location_name">> := _}, Entry)
    end},
    {iolist_to_binary([Label, " parking info"]), fun() ->
      ?assertMatch(#{<<"parking_info">> := _}, First)
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
