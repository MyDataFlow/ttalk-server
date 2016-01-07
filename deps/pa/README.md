# Partial application of Erlang functions

## Trivial example

An example says more than a thousand words:

```erlang
> Times2 = pa:bind(fun erlang:'*'/2, 2).
#Fun<pa.17.35850360>
> lists:map(Times2, [1,2,3]).
[2,4,6]
```

## Real world example

Take [an example from MongooseIM (actually a legacy leftover from ejabberd)][1] -
this _inline fun_ is 55 lines long!

[1]: https://github.com/esl/MongooseIM/blob/4211e4ca6e11d60cccfc0da16f08ba0c0d8e4a94/apps/ejabberd/src/mod_muc_room.erl#L2225-L2281

The fun code has problems with indentation, since the body of the 
inline fun should be indented more than the outer code and using `case`s 
and `if`s increases the indentation level even more.

This fun is complex and not easily traceable, because we don't know its real 
name (at least without referring to `erlc -S`) - in case of a fun this 
long, its logic might not be obvious, so tracing and seeing the return 
value might be really helpful.

A more general example of the same problem looks like this:

```erlang
do_something_on_a_list_of_items(ListOfItems) ->
    SomeVar1 = get_some_var1(),
    SomeVar2 = get_some_var2(),
    SomeVar3 = get_some_var3(),
    lists:map(fun (Elem) ->
                      %% very long closure using variables
                      %% SomeVar1, SomeVar2, SomeVar3 closed over
                      %% from the outer environment
              end, ListOfItems).
```

I hope you're convinced that refactoring is necessary.

```erlang
do_something_on_a_list_of_items(ListOfItems) ->
    SomeVar1 = get_some_var1(),
    SomeVar2 = get_some_var2(),
    SomeVar3 = get_some_var3(),
    lists:map(mk_step(SomeVar1, SomeVar2, SomeVar3), ListOfItems).

mk_step(SomeVar1, SomeVar2, SomeVar3) ->
    fun (Elem) ->
            do_something_with_one_item(SomeVar1, SomeVar2, SomeVar3, Elem)
    end.

do_something_with_one_item(SomeVar1, SomeVar2, SomeVar3, Elem) ->
    %% still a long function using variables
    %% SomeVar1, SomeVar2, SomeVar3
    %% passed in as arguments
    ...
```

The end result if functionally the same,
but `do_something_with_one_item/4` is perfectly traceable and doesn't suffer
from _pathological indentosis_.

However, `mk_step/3` from above is just boilerplate.
The same could be achieved without proliferation of similar `mk_sth/3`
and `mk_sth_else/5` functions throughout the codebase with function partial
application. Which Erlang lacks!

Thankfully, this library fixes this nuisance by providing a quite
convenient (as far as the syntax permits) implementation:

```erlang
do_something_on_a_list_of_items(ListOfItems) ->
    SomeVar1 = get_some_var1(),
    SomeVar2 = get_some_var2(),
    SomeVar3 = get_some_var3(),
    lists:map(pa:bind(fun do_something_with_one_item/4,
                      SomeVar1, SomeVar2, SomeVar3), ListOfItems).

do_something_with_one_item(SomeVar1, SomeVar2, SomeVar3, Elem) ->
    %% still a long function using variables
    %% SomeVar1, SomeVar2, SomeVar3
    %% passed in as arguments
    ...
```

## There is more than that!

Of course, `map/2` and `foreach/2` require unary functions,
but `foldl/3` a binary one and you might come up with a number of
functions with **even wilder** requirements!

You can define the maximum supported arity by defining the value of the
```max_partial_arity``` macro during compilation.

Defining it in your ```rebar.config``` file is a pretty convenient way to do so.

The default value is **15**.
