%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024-2026, Ralf Thomas Pietsch
%%% @doc File I/O for the output JSON: paths, open/close, streaming
%%% writes, reading existing meta data for update checks, and gzip
%%% compression.
%%%
%%% The file handle is wrapped in an opaque {@link lsl_output()} record
%%% so that callers never touch the raw `io_device()' directly.
%%%
%%% Paths and directories are read from the application environment
%%% (see `lsl.app.src').
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_output).

%% API
-export([open/0, open/1, close/1]).
-export([write_begin/1, write_data_separator/1, write_end/1]).
-export([write_json/2, write_meta/3]).
-export([read_existing_meta/0]).
-export([compress/0]).

-record(lsl_output, {io :: file:io_device()}).
-opaque lsl_output() :: #lsl_output{}.
-export_type([lsl_output/0]).

%%%===================================================================
%%% API
%%%===================================================================


%% @doc Opens the default output JSON file for writing.
-spec open() -> {ok, lsl_output()} | {error, term()}.
open() ->
  ensure_output_dir(),
  open_file(get_env(output_file)).

%% @doc Opens an arbitrary file for writing (e.g. for testing).
-spec open(string()) -> {ok, lsl_output()} | {error, term()}.
open(Path) ->
  open_file(Path).

%% @doc Closes the output file.
-spec close(lsl_output()) -> ok | {error, term()}.
close(#lsl_output{io = IO}) ->
  file:close(IO).

%% @doc Writes the opening brace of the JSON object.
-spec write_begin(lsl_output()) -> ok | {error, term()}.
write_begin(#lsl_output{io = IO}) ->
  file:write(IO, <<${, 10>>).

%% @doc Writes a comma + newline separator between JSON fields.
-spec write_data_separator(lsl_output()) -> ok | {error, term()}.
write_data_separator(#lsl_output{io = IO}) ->
  file:write(IO, <<$,, 10>>).

%% @doc Writes the closing brace of the JSON object.
-spec write_end(lsl_output()) -> ok | {error, term()}.
write_end(#lsl_output{io = IO}) ->
  file:write(IO, <<10, $}, 10>>).

%% @doc Writes an already-encoded JSON binary to the file.
-spec write_json(lsl_output(), binary()) -> ok | {error, term()}.
write_json(#lsl_output{io = IO}, JsonBin) ->
  file:write(IO, JsonBin).

%% @doc Writes the meta object to the JSON file.
-spec write_meta(lsl_output(), string(), string() | undefined) -> ok | {error, term()}.
write_meta(#lsl_output{io = IO}, Url, LastModified) ->
  file:write(IO, <<",\n">>),
  file:write(IO, lsl_json:encode_field(<<"meta">>, lsl_json:build_meta(Url, LastModified))),
  ok.

%% @doc Reads the "meta" object from the existing output JSON file.
-spec read_existing_meta() -> {ok, map()} | {error, term()}.
read_existing_meta() ->
  case read_json_bin() of
    {ok, Bin} ->
      case lsl_json:decode(Bin) of
        {ok, Map} ->
          case maps:get(<<"meta">>, Map, undefined) of
            undefined -> {error, no_meta};
            Meta -> {ok, Meta}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Compresses the output JSON file with gzip, creating a `.json.gz` file.
%% The uncompressed file is deleted afterwards so that only the `.gz`
%% version remains in the output directory.
-spec compress() -> ok | {error, term()}.
compress() ->
  OutputFile = get_env(output_file),
  OutputFileGz = get_env(output_file_gz),
  case file:read_file(OutputFile) of
    {ok, Data} ->
      Compressed = zlib:gzip(Data),
      case file:write_file(OutputFileGz, Compressed) of
        ok ->
          SizeOrig = byte_size(Data),
          SizeGz = byte_size(Compressed),
          io:fwrite("compressed ~p bytes -> ~p bytes (~.1f%)~n",
                    [SizeOrig, SizeGz, SizeGz * 100.0 / max(1, SizeOrig)]),
          file:delete(OutputFile),
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

%% Ensures the output directory exists.
ensure_output_dir() ->
  Dir = get_env(output_dir),
  ok = filelib:ensure_dir(Dir ++ "/"),
  file:make_dir(Dir),
  ok.

%% Opens a file and wraps the handle in the lsl_output record.
open_file(Path) ->
  case file:open(Path, [binary, write]) of
    {ok, IO} -> {ok, #lsl_output{io = IO}};
    {error, Reason} -> {error, Reason}
  end.

%% Reads the JSON content, trying the gzip version first.
read_json_bin() ->
  OutputFileGz = get_env(output_file_gz),
  OutputFile = get_env(output_file),
  case file:read_file(OutputFileGz) of
    {ok, GzData} ->
      try
        {ok, zlib:gunzip(GzData)}
      catch
        _:_ -> {error, invalid_gzip}
      end;
    {error, _} ->
      file:read_file(OutputFile)
  end.

%% Reads a value from the lsl application environment.
get_env(Key) ->
  {ok, Value} = application:get_env(lsl, Key),
  Value.
