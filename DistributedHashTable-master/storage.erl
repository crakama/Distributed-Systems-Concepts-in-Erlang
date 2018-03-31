-module(storage).
-export([create/0,add/3,lookup/2,split/3,merge/2]).

create() ->
    [].

  % add a key value pair, return the updated store
add(Key, Value, Store) ->
    lists:keystore(Key, 1, Store, {Key, Value}).

% return a tuple {Key, Value} or the atom false
lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).
% return a tuple {Updated, Rest} where the updated store only contains
% the key value pairs requested and the rest are found in a list of key-value pairs
split(From, To, Store) ->
    split(From, To, Store, [], []).

split(_, _, [], Updated, Rest) ->
    {Updated, Rest};

split(From, To, [{Key, Value}|Store], Updated, Rest) ->
    case key:between(Key, From, To) of
        true ->
            split(From, To, Store, [{Key, Value}|Updated], Rest);
        false ->
            split(From, To, Store, Updated, [{Key, Value}|Rest])
    end.

% add a list of key-value pairs to a store
merge(Key_Values, Store) ->
  lists:append(Key_Values,Store).
