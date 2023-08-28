%%%-------------------------------------------------------------------
%% @doc xlsx2erl public API
%% @end
%%%-------------------------------------------------------------------

-module(xlsx2erl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:start(ppool),
    xlsx2erl_sup:start_link().

stop(_State) ->
    init:stop(),
    ok.
