-module(key).
-export([generate/0,between/3]).

generate()->
	random:uniform(1000000000).


between(Key, From, To) ->
    if
        From < To ->
            (Key > From) and (Key =< To);

        From > To ->  % (1stNode/Successor)
            (Key > From) or (Key =< To);

        From == To -> % (1stNode/Successor)
            true
    end.


	%  Returns true or false.
	% ---------------Notify-------------------------
	% Key = ProposedPredkey/New Node
	% From = CurrentPred_key/ Existing node, Could be a succesor or
	% To = Id, a key of the currently processed Node
	% true -> If ProposedPredkey finds that is prposing to itself as pred(=) , or less than succ but greater than CurrentPred
	% false -> If neither of the above satisfies, then current pred will remain CurrentPred,
	% and that CurrentPred is the first node in the ring(1st Node ID is always => Id)
	% ---------------stabilize-------------------------
	% Key = CurrentPred_key
	% From = ProposedPredId
	% To = Succ_key
	% True = if Key(CurrentPred_key) < To(Succ_key), and > From ProposedPredId
	% False = Otherwise
