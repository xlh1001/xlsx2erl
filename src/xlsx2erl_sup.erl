%%%-------------------------------------------------------------------
%% @doc xlsx2erl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(xlsx2erl_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{
            id => xlsx2erl, 
            start => {xlsx2erl, start_link, []},
            restart => transient,
            shutdown => 5000,
            type => worker,
            modules => [xlsx2erl]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
