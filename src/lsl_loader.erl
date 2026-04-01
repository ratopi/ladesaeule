%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024-2026, Ralf Thomas Pietsch
%%% @doc HTTP communication: scrapes the CSV URL from the
%%% Bundesnetzagentur website, checks for updates via HEAD request,
%%% and downloads the CSV via streaming.
%%%
%%% For output (JSON file writing) this module delegates exclusively
%%% to lsl_output.
%%% @end
%%%-------------------------------------------------------------------
-module(lsl_loader).

%% API
-export([get_url/0, needs_update/1, load_data/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Scrapes the Bundesnetzagentur E-Mobility page and extracts
%% the URL of the current Ladesäulenregister CSV file.
-spec get_url() -> {ok, string()} | {error, term()}.
get_url() ->
  {ok, UrlUrl} = application:get_env(lsl, source_page_url),
  case httpc:request(UrlUrl) of
    {ok, {{_, 200, _}, _, Body}} ->
      case re:run(Body, "<a[^>]*href=\"([^>\"]*[.]csv)\"[^>]*>") of
        {match, [{_, _}, {Start, Length}]} ->
          {ok, lists:sublist(Body, Start + 1, Length)};
        _ ->
          {error, {csv_file, url_not_found}}
      end;
    {ok, {Head, _, _}} ->
      {error, {Head, UrlUrl}};
    Err ->
      {error, Err}
  end.


%% @doc Checks whether the CSV at `Url' has changed compared to the
%% existing output JSON. Returns `true' if an update is needed,
%% `false' if the data is unchanged.
%% Compares the URL and the `Last-Modified' HTTP header.
-spec needs_update(string()) -> boolean().
needs_update(Url) ->
  case lsl_output:read_existing_meta() of
    {ok, Meta} ->
      ExistingSource = maps:get(<<"source">>, Meta, undefined),
      ExistingLastMod = maps:get(<<"source_last_modified">>, Meta, undefined),
      UrlBin = list_to_binary(Url),
      case ExistingSource =:= UrlBin of
        false ->
          io:fwrite("URL changed~n"),
          true;
        true ->
          case get_last_modified(Url) of
            {ok, LastModified} ->
              LastModBin = list_to_binary(LastModified),
              case ExistingLastMod =:= LastModBin of
                true ->
                  io:fwrite("Last-Modified unchanged: ~s~n", [LastModified]),
                  false;
                false ->
                  io:fwrite("Last-Modified changed: ~s -> ~s~n", [ExistingLastMod, LastModified]),
                  true
              end;
            {error, _} ->
              %% Can't determine Last-Modified, update to be safe
              true
          end
      end;
    {error, _} ->
      %% No existing file or not readable, update needed
      true
  end.


%% @doc Downloads the CSV from `Url' via HTTP streaming, converts it
%% and writes the result to the output JSON file.
-spec load_data(string()) -> ok | {error, term()}.
load_data(Url) ->
  case lsl_output:open() of
    {ok, Out} ->
      lsl_output:write_begin(Out),
      io:fwrite("loading ~p~n", [Url]),
      case httpc:request(get, {Url, []}, [], [{sync, false}, {stream, self}, {body_format, binary}]) of
        Err = {error, _} ->
          lsl_output:close(Out),
          Err;
        {ok, RequestId} ->
          case stream_content(RequestId, lsl_converter:new_parser(Out)) of
            Err = {error, _} ->
              lsl_output:close(Out),
              Err;
            {ok, _Result, HttpHeaders} ->
              LastModified = proplists:get_value("last-modified", HttpHeaders, undefined),
              lsl_output:write_meta(Out, Url, LastModified),
              lsl_output:write_end(Out),
              lsl_output:close(Out),
              lsl_output:compress(),
              ok
          end
      end;
    {error, Reason} ->
      io:fwrite("Error opening output file: ~p~n", [Reason]),
      {error, {open_file, Reason}}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Performs a HEAD request to get the Last-Modified header without
%% downloading the full file.
get_last_modified(Url) ->
  case httpc:request(head, {Url, []}, [], []) of
    {ok, {{_, 200, _}, Headers, _}} ->
      case proplists:get_value("last-modified", Headers, undefined) of
        undefined -> {error, no_last_modified};
        Value -> {ok, Value}
      end;
    {ok, {{_, Code, _}, _, _}} ->
      {error, {http_status, Code}};
    {error, Reason} ->
      {error, Reason}
  end.


%% Receives HTTP stream messages and feeds them to the CSV parser.
stream_content(RequestId, CellParserFun) ->
  stream_content(RequestId, CellParserFun, []).

stream_content(RequestId, CellParserFun, HttpHeaders) ->
  {ok, Timeout} = application:get_env(lsl, stream_timeout),
  receive
    {http, {RequestId, stream_start, Headers}} ->
      stream_content(RequestId, CellParserFun, Headers);

    {http, {RequestId, stream, BinBodyPart}} ->
      case CellParserFun(BinBodyPart) of
        NextFun when is_function(NextFun) ->
          stream_content(RequestId, NextFun, HttpHeaders);
        Error ->
          {error, {parser_error, Error}}
      end;

    {http, {RequestId, stream_end, _TrailerHeaders}} ->
      {ok, CellParserFun(eof), HttpHeaders};

    {http, {RequestId, {{_, 404, _}, _Headers, _Content}}} ->
      {error, not_found}

  after Timeout ->
    {error, timeout}
  end.
