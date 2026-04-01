%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2026, Ralf Thomas Pietsch
%%% @doc Converts the BNetzA opening hours fields into the
%%% OpenStreetMap `opening_hours' format.
%%%
%%% Input (from the CSV):
%%%   Hours    :: binary()  – e.g. <<"247">>, <<"Keine Angabe">>
%%%   Weekdays :: binary()  – e.g. <<"Montag; Dienstag; Mittwoch; ...">>
%%%   Times    :: binary()  – e.g. <<"08:00-18:00; 08:00-18:00; ...">>
%%%
%%% Output:
%%%   {ok, binary()}  – the OSM opening_hours string
%%%   undefined       – when the data is insufficient
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_opening_hours).

-export([to_osm/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Convert BNetzA opening hours to OSM format.
-spec to_osm(binary(), binary(), binary()) -> {ok, binary()} | undefined.
to_osm(<<"247">>, _, _) ->
  {ok, <<"24/7">>};

to_osm(<<"Keine Angabe">>, _, _) ->
  undefined;

to_osm(<<>>, _, _) ->
  undefined;

to_osm(_, Weekdays, Times) when Weekdays =:= <<>>; Times =:= <<>> ->
  undefined;

to_osm(_, Weekdays, Times) ->
  Days = split_trim(Weekdays),
  Slots = split_trim(Times),
  case length(Days) =:= length(Slots) of
    false ->
      undefined;
    true ->
      DaySlots = lists:zip(
        lists:map(fun day_to_osm/1, Days),
        lists:map(fun normalise_time/1, Slots)
      ),
      case DaySlots of
        [] ->
          undefined;
        _ ->
          Grouped = group_consecutive(DaySlots),
          Parts = lists:map(fun format_group/1, Grouped),
          {ok, join(Parts, <<"; ">>)}
      end
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% --- Split semicolon-separated values and trim whitespace ---

split_trim(Bin) ->
  [string:trim(S) || S <- string:split(Bin, <<"; ">>, all), string:trim(S) =/= <<>>].

%% --- German weekday to OSM abbreviation ---

day_to_osm(<<"Montag">>)     -> <<"Mo">>;
day_to_osm(<<"Dienstag">>)   -> <<"Tu">>;
day_to_osm(<<"Mittwoch">>)   -> <<"We">>;
day_to_osm(<<"Donnerstag">>) -> <<"Th">>;
day_to_osm(<<"Freitag">>)    -> <<"Fr">>;
day_to_osm(<<"Samstag">>)    -> <<"Sa">>;
day_to_osm(<<"Sonntag">>)    -> <<"Su">>;
day_to_osm(Other)            -> Other.

%% --- Normalise time slot: 23:59 → 24:00 ---

normalise_time(<<"00:00-23:59">>) -> <<"00:00-24:00">>;
normalise_time(Slot) ->
  case binary:match(Slot, <<"-23:59">>) of
    {Start, 6} ->
      Prefix = binary:part(Slot, 0, Start),
      <<Prefix/binary, "-24:00">>;
    _ ->
      Slot
  end.

%% --- Group consecutive days with the same time slot ---

group_consecutive([]) ->
  [];
group_consecutive([{Day, Time} | Rest]) ->
  group_consecutive(Rest, Day, Day, Time, []).

group_consecutive([{Day, Time} | Rest], Start, _End, Time, Acc) ->
  group_consecutive(Rest, Start, Day, Time, Acc);
group_consecutive([{Day, Time2} | Rest], Start, End, Time1, Acc) ->
  group_consecutive(Rest, Day, Day, Time2, [{Start, End, Time1} | Acc]);
group_consecutive([], Start, End, Time, Acc) ->
  lists:reverse([{Start, End, Time} | Acc]).

%% --- Format a group into OSM string ---

format_group({Day, Day, <<"00:00-24:00">>}) ->
  <<Day/binary, " 00:00-24:00">>;
format_group({Start, End, <<"00:00-24:00">>}) ->
  <<Start/binary, "-", End/binary, " 00:00-24:00">>;
format_group({Day, Day, Time}) ->
  <<Day/binary, " ", Time/binary>>;
format_group({Start, End, Time}) ->
  <<Start/binary, "-", End/binary, " ", Time/binary>>.

%% --- Join a list of binaries with a separator ---

join([], _Sep) -> <<>>;
join([H], _Sep) -> H;
join([H | T], Sep) ->
  lists:foldl(fun(B, Acc) -> <<Acc/binary, Sep/binary, B/binary>> end, H, T).

