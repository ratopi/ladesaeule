%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 28. Mär 2024 23:59
%%%-------------------------------------------------------------------
-module(lsl).

-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Escript entry point.
%% Fetches the current CSV URL from the Bundesnetzagentur website,
%% downloads the CSV and converts it to `public/ladesaeulen.json`.
-spec main([string()]) -> no_return().
main(_Args) ->
  application:ensure_started(kernel),
  application:ensure_started(stdlib),
  application:ensure_started(inets),
  application:ensure_started(jsx),
  ssl:start(),
  case lsl_loader:get_url() of
    {ok, Url} ->
      case lsl_loader:needs_update(Url) of
        false ->
          io:fwrite("no update needed~n"),
          erlang:halt(0);
        true ->
          case lsl_loader:load_data(Url) of
            ok ->
              io:fwrite("finished~n"),
              erlang:halt(0);
            Err = {error, _} ->
              io:fwrite("FAIL ~p~n", [Err]),
              erlang:halt(1)
          end
      end;
    Err ->
      io:fwrite("Can't get URL: ~p~n", [Err]),
      erlang:halt(1)
  end.

%%====================================================================
%% Internal functions
%%====================================================================
