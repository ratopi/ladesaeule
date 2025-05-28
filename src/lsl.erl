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

main(_Args) ->
  application:ensure_started(kernel),
  application:ensure_started(stdlib),
  application:ensure_started(inets),
  application:ensure_started(jsx),
  case lsl_data_converter:get_url() of
    {ok, Url} ->
      case lsl_data_converter:load_data(Url) of
        {ok, _, _} -> io:fwrite("finished~n");
        Err = {error, _} -> io:fwrite("FAIL ~p~n", [Err])
      end;
    Err ->
      io:fwrite("Can't get URL: ~p~n", [Err])
  end,
  erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
