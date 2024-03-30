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
-export([load_data/1]).

-record(starting, {io, infos = []}).
-record(got_header1, {io}).
-record(read_lines, {io, headers, first_line = true}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_data(Url) ->
	{ok, IO} = file:open("./ladesaeulen.json", [binary, write]),

	file:write(IO, <<${, 10>>),

	io:fwrite("loading ~p~n", [Url]),
	case httpc:request(get, {Url, []}, [], [{sync, false}, {stream, self}, {body_format, binary}]) of
		Err = {error, _} ->
			Err;
		{ok, RequestId} ->
			case parse_content(RequestId, cell_parser:start(fun handler_fun/2, #starting{io = IO})) of
				Err = {error, _} ->
					Err;
				{ok, Content, _Headers} ->
					io:fwrite("~p~n", [Content]),
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
	file:write(IO, <<$[, 10>>),
	#read_lines{headers = Headers, io = IO};


handler_fun(eof, #read_lines{io = IO}) ->
	file:write(IO, <<10, $], 10, $}>>),
	file:close(IO),
	undefined;

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
			{ok, CellParserFun(eof), Headers}

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
	build_map(Headers, Line, Fun(V, Map)).



build_headers(Headers) ->
	lists:map(fun set_fun/1, Headers).



set_fun(X = <<"Betreiber">>) -> standard_fun(operator);
set_fun(X = <<"Straße"/utf8>>) -> sub_map(addr, X);
set_fun(X = <<"Hausnummer">>) -> sub_map(addr, X);
set_fun(X = <<"Adresszusatz">>) -> sub_map(addr, X);
set_fun(X = <<"Postleitzahl">>) -> sub_map(addr, X);
set_fun(X = <<"Ort">>) -> sub_map(addr, X);
set_fun(X = <<"Bundesland">>) -> sub_map(addr, X);
set_fun(X = <<"Kreis/kreisfreie Stadt">>) -> sub_map(addr, X);
set_fun(X = <<"Breitengrad">>) -> sub_map(geo, lat);
set_fun(X = <<"Längengrad"/utf8>>) -> sub_map(geo, lon);
set_fun(X = <<"Inbetriebnahmedatum">>) -> sub_map(charging, X);
set_fun(X = <<"Nennleistung Ladeeinrichtung [kW]">>) -> sub_map(charging, X);
set_fun(X = <<"Art der Ladeeinrichung">>) -> sub_map(charging, X);
set_fun(X = <<"Anzahl Ladepunkte">>) -> ignore();
set_fun(X = <<"Steckertypen1">>) -> v_to_list(charing_point([charging, points, 1, plugs]));
set_fun(X = <<"Steckertypen2">>) -> v_to_list(charing_point([charging, points, 2, plugs]));
set_fun(X = <<"Steckertypen3">>) -> v_to_list(charing_point([charging, points, 3, plugs]));
set_fun(X = <<"Steckertypen4">>) -> v_to_list(charing_point([charging, points, 4, plugs]));
set_fun(X = <<"P1 [kW]">>) -> charing_point([charging, points, 1, kW]);
set_fun(X = <<"P2 [kW]">>) -> charing_point([charging, points, 2, kW]);
set_fun(X = <<"P3 [kW]">>) -> charing_point([charging, points, 3, kW]);
set_fun(X = <<"P4 [kW]">>) -> charing_point([charging, points, 4, kW]);
set_fun(X = <<"Public Key1">>) -> charing_point([charging, points, 1, pkey]);
set_fun(X = <<"Public Key2">>) -> charing_point([charging, points, 2, pkey]);
set_fun(X = <<"Public Key3">>) -> charing_point([charging, points, 3, pkey]);
set_fun(X = <<"Public Key4">>) -> charing_point([charging, points, 4, pkey]);
% unknown
set_fun(X) -> erlang:error({unknown_header, X}).



standard_fun(X) -> fun(V, Map) -> maps:put(X, V, Map) end.

sub_map(SubKey, Key) -> fun(V, Map) -> maps:put(SubKey, maps:put(Key, V, maps:get(SubKey, Map, #{})), Map) end.

charing_point(KeyPath) ->
	fun(V, Map) ->
		deep_put(KeyPath, V, Map)
	end.

ignore() -> fun(_, Map) -> Map end.



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
