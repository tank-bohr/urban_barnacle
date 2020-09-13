-module (urban_barnacle_server).
-include_lib("kernel/include/logger.hrl").

-export([
    start_link/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    url                   :: string(),
    name                  :: atom(),
    model                 :: list(),
    record                :: tuple(),
    http_timeout_seconds  :: non_neg_integer(),
    sync_interval_seconds :: non_neg_integer(),
    etag = "none"         :: string(),
    timer                 :: timer:tref() | undefined,
    backoff               :: backoff:backoff()
}).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

init(#{
    url                   := Url,
    name                  := Name,
    model                 := Model,
    record                := Record,
    ets_opts              := EtsOpts,
    http_timeout_seconds  := HttpTimeoutSeconds,
    sync_interval_seconds := SyncIntervalSeconds
}) ->
    self() ! sync,
    ets:new(Name, EtsOpts),
    State = #state{
        url                   = Url,
        name                  = Name,
        model                 = Model,
        record                = Record,
        http_timeout_seconds  = HttpTimeoutSeconds,
        sync_interval_seconds = SyncIntervalSeconds,
        backoff               = backoff:init(2, 10)
    },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sync, State) ->
    {noreply, sync(State)};
handle_info(_Info, State) ->
    {noreply, State}.

sync(#state{url = Url, http_timeout_seconds = HttpTimeoutSeconds} = State) ->
    Request = {Url, [
        {"If-None-Match", State#state.etag}
    ]},
    HttpOptions = [
        {timeout, timer:seconds(HttpTimeoutSeconds)}
    ],
    Options = [
        {body_format, binary}
    ],
    case httpc:request(get, Request, HttpOptions, Options) of
        {ok, Response} ->
            ?LOG_ERROR("Success request: ~s", [Url]),
            on_success_response(Response, State);
        {error, Reason} ->
            ?LOG_ERROR("Error while http request ~s: ~p", [Url, Reason]),
            on_error_response(Reason, State)
    end.

on_success_response(Response, #state{name = Name, sync_interval_seconds = SyncInterval} = State) ->
    {_, Backoff} = backoff:succeed(State#state.backoff),
    ETag = case Response of
        {{_, 200, _}, RespHeaders, Body} ->
            Data = jsx:decode(Body, [return_maps, {labels, binary}]),
            store(Data, Name, State#state.model, State#state.record),
            extract_etag_from_headers(RespHeaders, State#state.etag);
        {{_, 304, _}, RespHeaders, _Body} ->
            ?LOG_DEBUG("Resource `~p` didn't change", [Name]),
            extract_etag_from_headers(RespHeaders, State#state.etag);
        {{_, Status, _}, RespHeaders, Body} ->
            ?LOG_ERROR("Couldn't fetch resource `~p`: ~p", [Name, Status]),
            ?LOG_DEBUG("Headers: ~p~nBody: ~p", [RespHeaders, Body]),
            State#state.etag
    end,
    ?LOG_DEBUG("Next sync in ~p seconds", [SyncInterval]),
    {ok, TRef} = timer:send_after(timer:seconds(SyncInterval), sync),
    State#state{timer = TRef, backoff = Backoff, etag = ETag}.

on_error_response(_Reason, State) ->
    {Timeout, Backoff} = backoff:fail(State#state.backoff),
    ?LOG_DEBUG("Retry in ~p seconds", [Timeout]),
    {ok, TRef} = timer:send_after(timer:seconds(Timeout), sync),
    State#state{timer = TRef, backoff = Backoff}.

store(#{<<"data">> := Data}, Name, Model, Record) ->
    Compiled = emodel:compile(Model, tuple),
    Records = lists:filtermap(fun(Item) ->
        case emodel:from_map(Item, Record, Compiled) of
            {ok, ValidRecord} ->
                {true, ValidRecord};
            {error, Errors} ->
                [?LOG_WARNING("Invalid field ~s: ~p", [Field, Message]) ||
                    {Field, Message} <- Errors],
                false
        end
    end, Data),
    ets:insert(Name, Records).

extract_etag_from_headers(Headers, Default) ->
    LHeaders = [{string:lowercase(N), V} || {N, V} <- Headers],
    proplists:get_value("etag", LHeaders, Default).
