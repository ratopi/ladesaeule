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
	Url = "https://data.bundesnetzagentur.de/Bundesnetzagentur/SharedDocs/Downloads/DE/Sachgebiete/Energie/Unternehmen_Institutionen/E_Mobilitaet/Ladesaeulenregister.csv",
	lsl_data_converter:load_data(Url),
	erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
