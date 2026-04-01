%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024-2026, Ralf Thomas Pietsch
%%% @doc Thin wrapper around jsx for JSON encoding and decoding.
%%% Provides convenience functions for building and encoding the
%%% JSON structures used in the Ladesäulenregister output.
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_json).

%% API – encoding
-export([encode/1, encode_field/2]).

%% API – decoding
-export([decode/1]).

%% API – meta object builder
-export([build_meta/2]).

%%%===================================================================
%%% API – encoding
%%%===================================================================

%% @doc Encodes a term (map, list, binary, number …) to a JSON binary.
-spec encode(term()) -> binary().
encode(Term) ->
  jsx:encode(Term).

%% @doc Encodes a single JSON object field: `"Key":Value`.
%% Returns a binary like `<<"\"key\":…">>`.
-spec encode_field(binary(), term()) -> binary().
encode_field(Key, Value) ->
  KeyJson = jsx:encode(Key),
  ValJson = jsx:encode(Value),
  <<KeyJson/binary, $:, ValJson/binary>>.

%%%===================================================================
%%% API – decoding
%%%===================================================================

%% @doc Decodes a JSON binary into an Erlang map.
-spec decode(binary()) -> {ok, map()} | {error, term()}.
decode(Bin) ->
  try
    {ok, jsx:decode(Bin, [return_maps])}
  catch
    _:_ -> {error, invalid_json}
  end.

%%%===================================================================
%%% API – meta object builder
%%%===================================================================

%% @doc Builds the meta map with download time, source URL,
%% and optionally the Last-Modified HTTP header value.
-spec build_meta(string(), string() | undefined) -> map().
build_meta(Url, LastModified) ->
  {{Y, M, D}, {H, Mi, S}} = {erlang:date(), erlang:time()},
  Meta0 = #{
    download_time => <<
      (fnum(Y))/binary, $-,
      (fnum(M))/binary, $-,
      (fnum(D))/binary, $T,
      (fnum(H))/binary, $:,
      (fnum(Mi))/binary, $:,
      (fnum(S))/binary
    >>,
    source => list_to_binary(Url)
  },
  case LastModified of
    undefined -> Meta0;
    _ -> Meta0#{source_last_modified => list_to_binary(LastModified)}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

fnum(N) ->
  Bin = integer_to_binary(N),
  case N < 10 of
    true -> <<$0, Bin/binary>>;
    false -> Bin
  end.
