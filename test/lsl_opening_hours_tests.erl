%%%-------------------------------------------------------------------
%%% @doc Tests for lsl_opening_hours – BNetzA to OSM conversion.
%%%-------------------------------------------------------------------
-module(lsl_opening_hours_tests).

-include_lib("eunit/include/eunit.hrl").

%% --- 24/7 ---

twenty_four_seven_test() ->
  ?assertEqual({ok, <<"24/7">>},
    lsl_opening_hours:to_osm(<<"247">>, <<"Montag; Dienstag">>, <<"00:00-23:59; 00:00-23:59">>)).

twenty_four_seven_ignores_other_fields_test() ->
  ?assertEqual({ok, <<"24/7">>},
    lsl_opening_hours:to_osm(<<"247">>, <<>>, <<>>)).

%% --- Keine Angabe / empty ---

keine_angabe_test() ->
  ?assertEqual(undefined,
    lsl_opening_hours:to_osm(<<"Keine Angabe">>, <<>>, <<>>)).

empty_hours_test() ->
  ?assertEqual(undefined,
    lsl_opening_hours:to_osm(<<>>, <<>>, <<>>)).

missing_weekdays_test() ->
  ?assertEqual(undefined,
    lsl_opening_hours:to_osm(<<"sonstige">>, <<>>, <<"08:00-18:00">>)).

missing_times_test() ->
  ?assertEqual(undefined,
    lsl_opening_hours:to_osm(<<"sonstige">>, <<"Montag">>, <<>>)).

%% --- Full week, same time ---

full_week_same_time_test() ->
  ?assertEqual(
    {ok, <<"Mo-Su 00:00-24:00">>},
    lsl_opening_hours:to_osm(
      <<"sonstige">>,
      <<"Montag; Dienstag; Mittwoch; Donnerstag; Freitag; Samstag; Sonntag">>,
      <<"00:00-23:59; 00:00-23:59; 00:00-23:59; 00:00-23:59; 00:00-23:59; 00:00-23:59; 00:00-23:59">>
    )).

full_week_business_hours_test() ->
  ?assertEqual(
    {ok, <<"Mo-Su 08:00-20:00">>},
    lsl_opening_hours:to_osm(
      <<"sonstige">>,
      <<"Montag; Dienstag; Mittwoch; Donnerstag; Freitag; Samstag; Sonntag">>,
      <<"08:00-20:00; 08:00-20:00; 08:00-20:00; 08:00-20:00; 08:00-20:00; 08:00-20:00; 08:00-20:00">>
    )).

%% --- Different times per day group ---

weekday_weekend_split_test() ->
  ?assertEqual(
    {ok, <<"Mo-Fr 08:00-18:00; Sa-Su 10:00-16:00">>},
    lsl_opening_hours:to_osm(
      <<"sonstige">>,
      <<"Montag; Dienstag; Mittwoch; Donnerstag; Freitag; Samstag; Sonntag">>,
      <<"08:00-18:00; 08:00-18:00; 08:00-18:00; 08:00-18:00; 08:00-18:00; 10:00-16:00; 10:00-16:00">>
    )).

%% --- Single day ---

single_day_test() ->
  ?assertEqual(
    {ok, <<"Mo 09:00-17:00">>},
    lsl_opening_hours:to_osm(
      <<"sonstige">>,
      <<"Montag">>,
      <<"09:00-17:00">>
    )).

%% --- Mixed: some days different ---

mixed_times_test() ->
  ?assertEqual(
    {ok, <<"Mo-Th 08:00-20:00; Fr 08:00-22:00; Sa-Su 10:00-18:00">>},
    lsl_opening_hours:to_osm(
      <<"sonstige">>,
      <<"Montag; Dienstag; Mittwoch; Donnerstag; Freitag; Samstag; Sonntag">>,
      <<"08:00-20:00; 08:00-20:00; 08:00-20:00; 08:00-20:00; 08:00-22:00; 10:00-18:00; 10:00-18:00">>
    )).

%% --- Normalisation: 23:59 → 24:00 ---

normalise_2359_test() ->
  ?assertEqual(
    {ok, <<"Mo 00:00-24:00">>},
    lsl_opening_hours:to_osm(
      <<"sonstige">>,
      <<"Montag">>,
      <<"00:00-23:59">>
    )).

%% --- Mismatched lengths ---

mismatched_lengths_test() ->
  ?assertEqual(undefined,
    lsl_opening_hours:to_osm(
      <<"sonstige">>,
      <<"Montag; Dienstag">>,
      <<"08:00-18:00">>
    )).

