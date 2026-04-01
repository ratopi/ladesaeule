%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2026, Ralf Thomas Pietsch
%%% @doc Row-level conversion: takes a CSV data line (list of binaries)
%%% and a column-index map, and produces a JSON-ready map by walking
%%% the mapping tree from lsl_mapping.
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_row).

%% API
-export([create/1, line_to_map/2]).

-record(lsl_row, {col_index, mapping}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates an lsl_row state from the CSV header line.
-spec create([binary()]) -> #lsl_row{}.
create(Headers) ->
  ColIndex = maps:from_list(
    [{Name, Idx} || {Idx, Name} <- lists:enumerate(1, Headers)]
  ),
  #lsl_row{col_index = ColIndex, mapping = lsl_mapping:mapping()}.

%% @doc Converts a CSV data line into a map using the mapping tree.
-spec line_to_map(#lsl_row{}, [binary()]) -> map().
line_to_map(#lsl_row{col_index = ColIndex, mapping = Mapping}, Line) ->
  build_nodes(Mapping, ColIndex, Line).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% --- mapping tree walker ---

build_nodes(Nodes, ColIndex, Line) ->
  lists:foldl(
    fun(Node, Acc) ->
      case eval_node(Node, ColIndex, Line) of
        undefined -> Acc;
        {Key, Value} -> maps:put(Key, Value, Acc)
      end
    end,
    #{},
    Nodes
  ).

%% Sub-object: {Key, Children}
eval_node({Key, Children}, ColIndex, Line) when is_list(Children) ->
  Sub = build_nodes(Children, ColIndex, Line),
  case maps:size(Sub) of
    0 -> undefined;
    _ -> {Key, Sub}
  end;

%% Collect: {Key, {collect, From, To}, TemplateFun}
eval_node({Key, {collect, From, To}, TemplateFun}, ColIndex, Line) ->
  Points = lists:foldl(
    fun(N, Acc) ->
      Sub = build_nodes(TemplateFun(N), ColIndex, Line),
      case maps:size(Sub) of
        0 -> Acc;
        _ -> [Sub | Acc]
      end
    end,
    [],
    lists:seq(To, From, -1)
  ),
  case Points of
    [] -> undefined;
    _  -> {Key, Points}
  end;

%% Custom function: {Key, {'fun', Fun}, CsvColumns}
eval_node({Key, {'fun', Fun}, CsvCols}, ColIndex, Line) ->
  ColMap = maps:from_list(
    [{Col, get_col(Col, ColIndex, Line)} || Col <- CsvCols]
  ),
  case Fun(ColMap) of
    undefined -> undefined;
    Value -> {Key, Value}
  end;

%% Leaf: {Key, Type, CsvColumn}
eval_node({Key, Type, CsvCol}, ColIndex, Line) ->
  case get_col(CsvCol, ColIndex, Line) of
    <<>> -> undefined;
    RawValue ->
      Value = clean(RawValue),
      {Key, convert(Type, Value)}
  end.


%% --- column access ---

get_col(ColName, ColIndex, Line) ->
  case maps:get(ColName, ColIndex, undefined) of
    undefined -> <<>>;
    Idx when Idx =< length(Line) -> lists:nth(Idx, Line);
    _ -> <<>>
  end.


%% --- value cleaning ---

clean(V) ->
  re:replace(V, <<"(", 194, 160, ")|(\n)$">>, <<>>, [{return, binary}]).


%% --- type conversion ---

convert(string, V) -> V;
convert(integer, V) -> to_integer(V);
convert(float, V) -> to_float(V);
convert(date, V) -> to_iso_date(V);
convert(list, V) -> split_list(V);
convert({list, ElemType}, V) ->
  lists:map(fun(E) -> convert(ElemType, E) end, split_list(V));
convert({map, Fun}, V) -> Fun(V);
convert({split_map, Fun}, V) ->
  [Fun(string:trim(P)) || P <- string:split(V, <<$;>>, all),
                           string:trim(P) =/= <<>>].


%% --- value converters ---

to_integer(V) when is_binary(V) -> binary_to_integer(V);
to_integer(V) -> V.

to_float(V) when is_binary(V) ->
  case string:split(V, <<$,>>) of
    [A, B] -> binary_to_float(<<A/binary, $., B/binary>>);
    [_]    -> binary_to_integer(V)
  end;
to_float(V) -> V.

to_iso_date(<<D1, D2, $., M1, M2, $., Y1, Y2, Y3, Y4>>) ->
  <<Y1, Y2, Y3, Y4, $-, M1, M2, $-, D1, D2>>;
to_iso_date(V) -> V.

split_list(V) ->
  string:split(V, <<"; ">>, all).
