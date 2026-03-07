%%%-------------------------------------------------------------------
%%% @doc Tests for lsl_data_converter using a sample CSV file.
%%%-------------------------------------------------------------------
-module(lsl_data_converter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SAMPLE_CSV, "test/data/ladesaeulen_sample.csv").
-define(OUTPUT_JSON, "test/data/test_output.json").

%% --- Helper ---

convert_sample() ->
  {ok, _} = lsl_data_converter:convert_file(?SAMPLE_CSV, ?OUTPUT_JSON),
  {ok, Bin} = file:read_file(?OUTPUT_JSON),
  jsx:decode(Bin, [return_maps]).

%% --- Tests ---

convert_produces_valid_json_test() ->
  Map = convert_sample(),
  ?assert(is_map(Map)),
  ?assertMatch(#{<<"infos">> := _, <<"data">> := _, <<"meta">> := _}, Map).

infos_contain_header_lines_test() ->
  #{<<"infos">> := Infos} = convert_sample(),
  ?assert(is_list(Infos)),
  ?assert(length(Infos) > 0),
  %% First info line should be the register title
  [First | _] = Infos,
  ?assertNotEqual(nomatch, binary:match(First, <<"Ladesäulenregister"/utf8>>)).

data_has_expected_entry_count_test() ->
  #{<<"data">> := Data} = convert_sample(),
  %% The sample CSV has 19 data rows (lines 12-30)
  ?assertEqual(19, length(Data)).

meta_contains_source_test() ->
  #{<<"meta">> := Meta} = convert_sample(),
  ?assertMatch(#{<<"source">> := _, <<"download_time">> := _}, Meta).

first_entry_has_all_general_fields_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  %% All "Allgemeine Informationen" fields should be present
  ?assertMatch(#{<<"id">> := _}, First),
  ?assertMatch(#{<<"operator">> := _}, First),
  ?assertMatch(#{<<"status">> := _}, First),
  ?assertMatch(#{<<"device_type">> := _}, First),
  ?assertMatch(#{<<"addr">> := _}, First),
  ?assertMatch(#{<<"geo">> := _}, First),
  ?assertMatch(#{<<"charging">> := _}, First).

first_entry_address_fields_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  #{<<"addr">> := Addr} = First,
  ?assertMatch(#{<<"Straße"/utf8>> := _}, Addr),
  ?assertMatch(#{<<"Hausnummer">> := _}, Addr),
  ?assertMatch(#{<<"Postleitzahl">> := _}, Addr),
  ?assertMatch(#{<<"Ort">> := _}, Addr),
  ?assertMatch(#{<<"Bundesland">> := _}, Addr),
  ?assertMatch(#{<<"Kreis/kreisfreie Stadt">> := _}, Addr).

first_entry_geo_is_numeric_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  #{<<"geo">> := Geo} = First,
  #{<<"lat">> := Lat, <<"lon">> := Lon} = Geo,
  ?assert(is_number(Lat)),
  ?assert(is_number(Lon)).

first_entry_charging_points_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  #{<<"charging">> := Charging} = First,
  ?assertMatch(#{<<"points">> := _}, Charging),
  #{<<"points">> := Points} = Charging,
  ?assert(is_list(Points)),
  %% First entry has 2 charging points
  ?assertEqual(2, length(Points)).

charging_point_has_plugs_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  #{<<"charging">> := #{<<"points">> := [P1 | _]}} = First,
  ?assertMatch(#{<<"plugs">> := _}, P1),
  #{<<"plugs">> := Plugs} = P1,
  ?assert(is_list(Plugs)),
  ?assert(length(Plugs) > 0).

charging_point_has_power_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  #{<<"charging">> := #{<<"points">> := [P1 | _]}} = First,
  ?assertMatch(#{<<"power">> := _}, P1).

charging_point_has_evse_id_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  #{<<"charging">> := #{<<"points">> := [P1 | _]}} = First,
  ?assertMatch(#{<<"evse_id">> := _}, P1).

charging_point_has_pkey_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  #{<<"charging">> := #{<<"points">> := [P1 | _]}} = First,
  ?assertMatch(#{<<"pkey">> := _}, P1).

entry_with_four_points_test() ->
  #{<<"data">> := Data} = convert_sample(),
  %% Entry 2 (index 1) has 4 charging points
  Second = lists:nth(2, Data),
  #{<<"charging">> := #{<<"points">> := Points}} = Second,
  ?assertEqual(4, length(Points)).

schnellladeeinrichtung_has_multiple_plug_types_test() ->
  #{<<"data">> := Data} = convert_sample(),
  %% Entry 6 (1025458) is a Schnellladeeinrichtung with multi-plug types
  Entry = lists:nth(6, Data),
  ?assertEqual(<<"Schnellladeeinrichtung">>, maps:get(<<"device_type">>, Entry)),
  #{<<"charging">> := #{<<"points">> := Points}} = Entry,
  ?assertEqual(2, length(Points)),
  %% Second point should have multiple plug types (DC Combo + CHAdeMO)
  [_P1, P2] = Points,
  #{<<"plugs">> := Plugs2} = P2,
  ?assertEqual(2, length(Plugs2)).

opening_hours_fields_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  %% First entry has opening hours info
  ?assertMatch(#{<<"opening_hours">> := _}, First),
  ?assertMatch(#{<<"opening_weekdays">> := _}, First),
  ?assertMatch(#{<<"opening_daytime">> := _}, First).

display_name_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  ?assertMatch(#{<<"display_name">> := _}, First).

payment_info_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  ?assertMatch(#{<<"payment">> := _}, First).

adresszusatz_test() ->
  #{<<"data">> := Data} = convert_sample(),
  %% Entry 7 (1120617) has Adresszusatz "Parkplatz Albhotel"
  Entry7 = lists:nth(7, Data),
  #{<<"addr">> := Addr7} = Entry7,
  ?assertMatch(#{<<"Adresszusatz">> := <<"Parkplatz Albhotel">>}, Addr7),
  %% Entry 8 (1121121) has Adresszusatz "Parkplatz Albhotel Bahnhoefle"
  Entry8 = lists:nth(8, Data),
  #{<<"addr">> := Addr8} = Entry8,
  ?assertMatch(#{<<"Adresszusatz">> := <<"Parkplatz Albhotel Bahnhoefle">>}, Addr8).

location_name_test() ->
  #{<<"data">> := Data} = convert_sample(),
  %% Entry 7 (1120617) has Standortbezeichnung "Albhotel Bahnhöfle"
  Entry = lists:nth(7, Data),
  ?assertMatch(#{<<"location_name">> := _}, Entry).

parking_info_test() ->
  #{<<"data">> := [First | _]} = convert_sample(),
  ?assertMatch(#{<<"parking_info">> := _}, First).

cleanup_test() ->
  file:delete(?OUTPUT_JSON).





