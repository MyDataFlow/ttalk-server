-module(pa_pt).

-export([parse_transform/2]).

-ifndef(max_partial_arity).
-define(max_partial_arity, 15).
-endif.

-define(FUNCTION, bind).

parse_transform(Forms, _Options) ->
    module(Forms, []).

module([{eof, _}=Eof], Acc) ->
    lists:reverse(Acc) ++ functions(?max_partial_arity) ++ [Eof];
module([{attribute, _, module, _}=Module|Tail], Acc) ->
    module(Tail, [exports(?max_partial_arity), Module | Acc]);
module([Head|Tail], Acc) ->
    module(Tail, [Head|Acc]).

exports(MaxArity) ->
    {attribute, 0, export,
     [{?FUNCTION, AppliedArgs+1} || AppliedArgs <- lists:seq(0, MaxArity)]}.

functions(MaxArity) ->
    [function(AppliedArgs, MaxArity) || AppliedArgs <- lists:seq(0, MaxArity)].

function(AppliedArgs, MaxArity) ->
    Clauses = [clause(FunArity, AppliedArgs)
               || FunArity <- lists:seq(AppliedArgs, MaxArity)],
    AllClauses = Clauses ++ [badarg_clause(AppliedArgs)],
    {function, 0, ?FUNCTION, AppliedArgs+1, AllClauses}.

badarg_clause(AppliedArgs) ->
    {clause, 0,
     [{var, 0, '_'} || _ <- lists:seq(0, AppliedArgs)],
     [],
     [{call, 0, {atom, 0, error}, [{atom, 0, badarg}]}]}.

clause(FunArity, AppliedArgs) ->
    {clause, 0,
     clause_header(AppliedArgs),
     clause_guard(FunArity),
     clause_body(FunArity, AppliedArgs)}.

clause_header(AppliedArgs) ->
    [{var, 0, 'Fun'} | args(1, AppliedArgs)].

clause_guard(FunArity) ->
    [[{call, 0,
       {atom, 0, 'is_function'},
       [{var, 0, 'Fun'}, {integer, 0, FunArity}]}]].

clause_body(FunArity, AppliedArgs) ->
    [{'fun', 0,
      {clauses,
       [{clause, 0,
         args(AppliedArgs+1, FunArity),
         [],
         [{call, 0,
           {var, 0, 'Fun'},
           args(1, FunArity)}]}]}}].

args(From, To) ->
    [{var, 0, arg(N)} || N <- lists:seq(From, To)].

arg(N) ->
    list_to_atom("A" ++ integer_to_list(N)).
