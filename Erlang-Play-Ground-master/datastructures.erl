-module(datastructures).
-export([people/0]).

people() ->
  io:format([{person,"kate","Rakama",
             [{shoesize,38},
             {pet,[{pup,"jimmy"},{cat, "micky"}]},
             {children,[{Bryn,7},{Daerneys,0}]}]
             },
             {person,"xteen","Rakama",
             [{shoesize,42},
             {pet,[{pup,"maxi"},{cat,"mia"}]},
             {children,[{Manu,7},{Easter,5}]}]}]).
