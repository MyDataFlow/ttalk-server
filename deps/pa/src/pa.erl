-module(pa).

-compile({parse_transform, pa_pt}).

% Parse transform generates set of functions bind/1-(N+1)
% that will be exported from this module.
%
% Value of N is set via macro 'max_partial_arity'.
% Default value is 15.
%
% The generated set of functions is equivalent to the following example
% (for N=3) accompanied by the corresponding AST that should be helpful
% to understand the parse transform.
%
%bind(Fun) when is_function(Fun, 0) ->
%    fun() -> Fun() end;
%bind(Fun) when is_function(Fun, 1) ->
%    fun(A1) -> Fun(A1) end;
%bind(Fun) when is_function(Fun, 2) ->
%    fun(A1, A2) -> Fun(A1, A2) end;
%bind(Fun) when is_function(Fun, 3) ->
%    fun(A1, A2, A3) -> Fun(A1, A2, A3) end;
%bind(_) ->
%    error(badarg).
%
%bind(Fun, A1) when is_function(Fun, 1) ->
%    fun() -> Fun(A1) end;
%bind(Fun, A1) when is_function(Fun, 2) ->
%    fun(A2) -> Fun(A1, A2) end;
%bind(Fun, A1) when is_function(Fun, 3) ->
%    fun(A2, A3) -> Fun(A1, A2, A3) end;
%bind(_, _) ->
%    error(badarg).
%
%bind(Fun, A1, A2) when is_function(Fun, 2) ->
%    fun() -> Fun(A1, A2) end;
%bind(Fun, A1, A2) when is_function(Fun, 3) ->
%    fun(A3) -> Fun(A1, A2, A3) end;
%bind(_, _, _) ->
%    error(badarg).
%
%bind(Fun, A1, A2, A3) when is_function(Fun, 3) ->
%    fun() -> Fun(A1, A2, A3) end;
%bind(_, _, _, _) ->
%    error(badarg).
%
%[{attribute,1,file,{"src/pa.erl",1}},
%    {attribute,1,module,pa},
%    {function,5,bind,1,
%     [{clause,5,
%       [{var,5,'Fun'}],
%       [[{call,5,{atom,5,is_function},[{var,5,'Fun'},{integer,5,0}]}]],
%       [{'fun',6,
%         {clauses,[{clause,6,[],[],[{call,6,{var,6,'Fun'},[]}]}]}}]},
%      {clause,7,
%       [{var,7,'Fun'}],
%       [[{call,7,{atom,7,is_function},[{var,7,'Fun'},{integer,7,1}]}]],
%       [{'fun',8,
%         {clauses,
%          [{clause,8,
%            [{var,8,'A1'}],
%            [],
%            [{call,8,{var,8,'Fun'},[{var,8,'A1'}]}]}]}}]},
%      {clause,9,
%       [{var,9,'Fun'}],
%       [[{call,9,{atom,9,is_function},[{var,9,'Fun'},{integer,9,2}]}]],
%       [{'fun',10,
%         {clauses,
%          [{clause,10,
%            [{var,10,'A1'},{var,10,'A2'}],
%            [],
%            [{call,10,
%              {var,10,'Fun'},
%              [{var,10,'A1'},{var,10,'A2'}]}]}]}}]},
%      {clause,11,
%       [{var,11,'Fun'}],
%       [[{call,11,{atom,11,is_function},[{var,11,'Fun'},{integer,11,3}]}]],
%       [{'fun',12,
%         {clauses,
%          [{clause,12,
%            [{var,12,'A1'},{var,12,'A2'},{var,12,'A3'}],
%            [],
%            [{call,12,
%              {var,12,'Fun'},
%              [{var,12,'A1'},
%               {var,12,'A2'},
%               {var,12,'A3'}]}]}]}}]},
%      {clause,13,
%       [{var,13,'_'}],
%       [],
%       [{call,14,{atom,14,error},[{atom,14,badarg}]}]}]},
%    {function,16,bind,2,
%     [{clause,16,
%       [{var,16,'Fun'},{var,16,'A1'}],
%       [[{call,16,{atom,16,is_function},[{var,16,'Fun'},{integer,16,1}]}]],
%       [{'fun',17,
%         {clauses,
%          [{clause,17,[],[],
%            [{call,17,{var,17,'Fun'},[{var,17,'A1'}]}]}]}}]},
%      {clause,18,
%       [{var,18,'Fun'},{var,18,'A1'}],
%       [[{call,18,{atom,18,is_function},[{var,18,'Fun'},{integer,18,2}]}]],
%       [{'fun',19,
%         {clauses,
%          [{clause,19,
%            [{var,19,'A2'}],
%            [],
%            [{call,19,
%              {var,19,'Fun'},
%              [{var,19,'A1'},{var,19,'A2'}]}]}]}}]},
%      {clause,20,
%       [{var,20,'Fun'},{var,20,'A1'}],
%       [[{call,20,{atom,20,is_function},[{var,20,'Fun'},{integer,20,3}]}]],
%       [{'fun',21,
%         {clauses,
%          [{clause,21,
%            [{var,21,'A2'},{var,21,'A3'}],
%            [],
%            [{call,21,
%              {var,21,'Fun'},
%              [{var,21,'A1'},
%               {var,21,'A2'},
%               {var,21,'A3'}]}]}]}}]},
%      {clause,22,
%       [{var,22,'_'},{var,22,'_'}],
%       [],
%       [{call,23,{atom,23,error},[{atom,23,badarg}]}]}]},
%    {function,25,bind,3,
%     [{clause,25,
%       [{var,25,'Fun'},{var,25,'A1'},{var,25,'A2'}],
%       [[{call,25,{atom,25,is_function},[{var,25,'Fun'},{integer,25,2}]}]],
%       [{'fun',26,
%         {clauses,
%          [{clause,26,[],[],
%            [{call,26,
%              {var,26,'Fun'},
%              [{var,26,'A1'},{var,26,'A2'}]}]}]}}]},
%      {clause,27,
%       [{var,27,'Fun'},{var,27,'A1'},{var,27,'A2'}],
%       [[{call,27,{atom,27,is_function},[{var,27,'Fun'},{integer,27,3}]}]],
%       [{'fun',28,
%         {clauses,
%          [{clause,28,
%            [{var,28,'A3'}],
%            [],
%            [{call,28,
%              {var,28,'Fun'},
%              [{var,28,'A1'},
%               {var,28,'A2'},
%               {var,28,'A3'}]}]}]}}]},
%      {clause,29,
%       [{var,29,'_'},{var,29,'_'},{var,29,'_'}],
%       [],
%       [{call,30,{atom,30,error},[{atom,30,badarg}]}]}]},
%    {function,32,bind,4,
%     [{clause,32,
%       [{var,32,'Fun'},{var,32,'A1'},{var,32,'A2'},{var,32,'A3'}],
%       [[{call,32,{atom,32,is_function},[{var,32,'Fun'},{integer,32,3}]}]],
%       [{'fun',33,
%         {clauses,
%          [{clause,33,[],[],
%            [{call,33,
%              {var,33,'Fun'},
%              [{var,33,'A1'},
%               {var,33,'A2'},
%               {var,33,'A3'}]}]}]}}]},
%      {clause,34,
%       [{var,34,'_'},{var,34,'_'},{var,34,'_'},{var,34,'_'}],
%       [],
%       [{call,35,{atom,35,error},[{atom,35,badarg}]}]}]},
%    {eof,36}]
