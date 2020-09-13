-module (urban_barnacle_server).
-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(SERVER, ?MODULE).

-record(pokemon, {
    name                :: binary(),
    hp              = 0 :: non_neg_integer(),
    speed           = 0 :: non_neg_integer(),
    attack          = 0 :: non_neg_integer(),
    defense         = 0 :: non_neg_integer(),
    special_attack  = 0 :: non_neg_integer(),
    special_defense = 0 :: non_neg_integer()
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

fetch() ->
    {ok, Response} = httpc:request("http://localhost:4000/api/pokemons")
    case Response of
        {{_, 200, _}, _Headers, Body} ->
            store(jsx:decode(Body, [return_maps, {labels, existing_atom}]));
        {{_, Status, _}, Headers, Body} ->
            ?LOG_ERROR("Couldn't fetch pokemons: ~p", [Status]),
            ?LOG_DEBUG("Headers ~p", [Headers]),
            ?LOG_DEBUG("Body ~p", [Body])
    end.

store(#{data := Pokemons}) ->
