%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024-2026, Ralf Thomas Pietsch
%%% @doc Manages the output JSON file: path, opening, writing meta
%%% data, reading existing meta for update checks.
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_json).

%% API
-export([output_path/0, ensure_output_dir/0, open/0, close/1]).
-export([write_begin/1, write_meta/3, write_end/1]).
-export([read_existing_meta/0, build_meta/2]).
-export([compress/0]).

-define(OUTPUT_DIR, "./public").
-define(OUTPUT_FILE, "./public/ladesaeulen.json").
-define(OUTPUT_FILE_GZ, "./public/ladesaeulen.json.gz").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns the path of the output JSON file.
-spec output_path() -> string().
output_path() -> ?OUTPUT_FILE.

%% @doc Ensures the output directory exists.
-spec ensure_output_dir() -> ok | {error, term()}.
ensure_output_dir() ->
  ok = filelib:ensure_dir(?OUTPUT_DIR ++ "/"),
  file:make_dir(?OUTPUT_DIR),
  ok.

%% @doc Opens the output JSON file for writing.
-spec open() -> {ok, file:io_device()} | {error, term()}.
open() ->
  ensure_output_dir(),
  file:open(?OUTPUT_FILE, [binary, write]).

%% @doc Closes the output file.
-spec close(file:io_device()) -> ok | {error, term()}.
close(IO) ->
  file:close(IO).

%% @doc Writes the opening brace of the JSON object.
-spec write_begin(file:io_device()) -> ok | {error, term()}.
write_begin(IO) ->
  file:write(IO, <<${, 10>>).

%% @doc Writes the meta object and closing brace to the JSON file.
-spec write_meta(file:io_device(), string(), string() | undefined) -> ok | {error, term()}.
write_meta(IO, Url, LastModified) ->
  file:write(IO, <<",\n">>),
  file:write(IO, <<"\"meta\":">>),
  file:write(IO, jsx:encode(build_meta(Url, LastModified))),
  ok.

%% @doc Writes the closing brace of the JSON object.
-spec write_end(file:io_device()) -> ok | {error, term()}.
write_end(IO) ->
  file:write(IO, <<10, $}, 10>>).

%% @doc Reads the "meta" object from the existing output JSON file.
-spec read_existing_meta() -> {ok, map()} | {error, term()}.
read_existing_meta() ->
  case read_json_bin() of
    {ok, Bin} ->
      try
        Map = jsx:decode(Bin, [return_maps]),
        case maps:get(<<"meta">>, Map, undefined) of
          undefined -> {error, no_meta};
          Meta -> {ok, Meta}
        end
      catch
        _:_ -> {error, invalid_json}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

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

%% @doc Compresses the output JSON file with gzip, creating a `.json.gz` file.
%% The uncompressed file is deleted afterwards so that only the `.gz`
%% version remains in the output directory.
-spec compress() -> ok | {error, term()}.
compress() ->
  case file:read_file(?OUTPUT_FILE) of
    {ok, Data} ->
      Compressed = zlib:gzip(Data),
      case file:write_file(?OUTPUT_FILE_GZ, Compressed) of
        ok ->
          SizeOrig = byte_size(Data),
          SizeGz = byte_size(Compressed),
          io:fwrite("compressed ~p bytes -> ~p bytes (~.1f%)~n",
                    [SizeOrig, SizeGz, SizeGz * 100.0 / max(1, SizeOrig)]),
          file:delete(?OUTPUT_FILE),
          ok;
        {error, Reason} ->
          {error, {write_gz, Reason}}
      end;
    {error, Reason} ->
      {error, {read_json, Reason}}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Reads the JSON content, trying the gzip version first.
read_json_bin() ->
  case file:read_file(?OUTPUT_FILE_GZ) of
    {ok, GzData} ->
      try
        {ok, zlib:gunzip(GzData)}
      catch
        _:_ -> {error, invalid_gzip}
      end;
    {error, _} ->
      %% Fall back to uncompressed file
      file:read_file(?OUTPUT_FILE)
  end.

fnum(N) ->
  Bin = integer_to_binary(N),
  case N < 10 of
    true -> <<$0, Bin/binary>>;
    false -> Bin
  end.

