%%%-------------------------------------------------------------------
%%% Created : 22 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
%%% This is the message top level supervisor
%%% @end
%%%-------------------------------------------------------------------

-module(message_sup).

-behaviour(supervisor).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1]).

%%====================================================================
%% Local Definitions
%%====================================================================

-define(SERVER, ?MODULE).
-define(MSG_SERVER_NAME, message).

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 4,
               period => 30},

  ChildSpecs = [#{id => ?MSG_SERVER_NAME,
                start => {?MSG_SERVER_NAME, start_link, []},
                restart => permanent,
                type => supervisor,
                shutdown => brutal_kill}],

  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

