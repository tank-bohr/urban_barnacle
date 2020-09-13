-module (urban_barnacle_server).
-include_lib("kernel/include/logger.hrl").

-include("pokemons.hrl").

-export([
    start_link/0
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(SERVER, ?MODULE).
-define(URL, "http://localhost:4000/api/pokemons").
-define(SYNC_INTERVAL_SECONDS, 60).
-define(HTTP_TIMEOUT_SECONDS, 5).

-record(state, {
    etag = "none" :: string(),
    timer         :: timer:tref() | undefined
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
    ok = logger:set_primary_config(level, debug),
    pokemons = ets:new(pokemons, [named_table, public, set, {keypos, #pokemon.name}]),
    self() ! sync,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sync, State) ->
    {noreply, sync(State)};
handle_info(_Info, State) ->
    {noreply, State}.

sync(State) ->
    {ok, Response} = httpc:request(get,
        {?URL, [{"If-None-Match", State#state.etag}]},
        [{timeout, timer:seconds(?HTTP_TIMEOUT_SECONDS)}],
        [{body_format, binary}]),
    ETag = case Response of
        {{_, 200, _}, RespHeaders, Body} ->
            store(jsx:decode(Body, [return_maps, {labels, binary}])),
            extract_etag_from_headers(RespHeaders, State#state.etag);
        {{_, 304, _}, RespHeaders, _Body} ->
            ?LOG_DEBUG("304 (Not Modified)"),
            extract_etag_from_headers(RespHeaders, State#state.etag);
        {{_, Status, _}, RespHeaders, Body} ->
            ?LOG_ERROR("Couldn't fetch pokemons: ~p", [Status]),
            ?LOG_DEBUG("Headers: ~p~nBody: ~p", [RespHeaders, Body]),
            State#state.etag
    end,
    {ok, TRef} = timer:send_after(timer:seconds(?SYNC_INTERVAL_SECONDS), sync),
    State#state{timer = TRef, etag = ETag}.

store(#{<<"data">> := Data}) ->
    Model = emodel:compile(?POKEMON_MODEL, tuple),
    Pokemons = lists:filtermap(fun(Item) -> from_map(Item, Model) end, Data),
    ets:insert(pokemons, Pokemons).

from_map(Item, Model) ->
    case emodel:from_map(Item, #pokemon{}, Model) of
        {ok, Pokemon} ->
            {true, Pokemon};
        {error, Errors} ->
            [?LOG_WARNING("Invalid field ~s: ~p", [Field, Message]) ||
                {Field, Message} <- Errors],
            false
    end.

extract_etag_from_headers(Headers, Default) ->
    LHeaders = [{string:lowercase(N), V} || {N, V} <- Headers],
    proplists:get_value("etag", LHeaders, Default).
