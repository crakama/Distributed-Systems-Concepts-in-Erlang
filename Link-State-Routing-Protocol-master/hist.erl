
-module(hist).
-export([new/1, update/3]).

% Return a new history, where messages from Name will
% always be seen as old
new(Name) ->
    NewHistory = dict:new(),
    dict:append(Name, inf, NewHistory).


% Determines if a link-state message is old or new
update(Node, N, History) ->
    case dict:find(Node, History) of
        {ok,[Value|_]} ->
            if
                N > Value -> % Update dict if N is greater
                    InitialHistory = dict:erase(Node, History),
                    UpdatedHistory = dict:append(Node, N, InitialHistory),
                    {new, UpdatedHistory};
                true ->
                    old
            end;
        error ->
            {new, dict:append(Node, N, History)}
    end.
