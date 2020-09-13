-ifndef(POKEMONS_HRL).
-define(POKEMONS_HRL, true).

-record(pokemon, {
    name                :: binary(),
    hp              = 0 :: non_neg_integer(),
    speed           = 0 :: non_neg_integer(),
    attack          = 0 :: non_neg_integer(),
    defense         = 0 :: non_neg_integer(),
    special_attack  = 0 :: non_neg_integer(),
    special_defense = 0 :: non_neg_integer()
}).

-define(POKEMON_MODEL, [
    {<<"name">>,            required, string,  #pokemon.name,           [non_empty]},
    {<<"hp">>,              required, integer, #pokemon.hp,             [{'>', 0}] },
    {<<"speed">>,           required, integer, #pokemon.speed,          [{'>', 0}] },
    {<<"attack">>,          required, integer, #pokemon.attack,         [{'>', 0}] },
    {<<"defense">>,         required, integer, #pokemon.defense,        [{'>', 0}] },
    {<<"special_attack">>,  required, integer, #pokemon.special_attack, [{'>', 0}] },
    {<<"special_defense">>, required, integer, #pokemon.special_defense,[{'>', 0}] }
]).

-endif.
