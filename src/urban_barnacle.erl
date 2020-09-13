-module(urban_barnacle).

-include("pokemons.hrl").

-export([
  start_pokemons/0
]).

start_pokemons() ->
    Config = #{
        url                   => "http://localhost:4000/api/pokemons",
        name                  => pokemons,
        model                 => ?POKEMON_MODEL,
        record                => #pokemon{},
        ets_opts              => [named_table, public, set, {keypos, #pokemon.name}],
        http_timeout_seconds  => 5,
        sync_interval_seconds => 60
    },
    urban_barnacle_server:start_link(Config).
