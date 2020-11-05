:- module(tokenize_opts,
          [process_options/4,
           preopts_data/3,
           tokenopts_data/3,
           postopts_data/3]).

:- use_module(library(record)).


:- record preopts(
       cased:boolean=false
   ).

:- record tokenopts(
       numbers:boolean=true,
       strings:boolean=true
   ).


:- record postopts(
       spaces:boolean=true,
       cntrl:boolean=true,
       punct:boolean=true,
       to:oneof([strings,atoms,chars,codes])=atoms,
       pack:boolean=false
   ).



process_options(Options, PreOpts, TokenOpts, PostOpts) :-
    make_preopts(Options, PreOpts, Rest0),
    make_postopts(Rest0, PostOpts, Rest1),
    make_tokenopts(Rest1, TokenOpts, InvalidOpts),
    throw_on_invalid_options(InvalidOpts).

throw_on_invalid_options(InvalidOpts) :-
    InvalidOpts \= []
    -> throw(invalid_options_given(InvalidOpts))
    ;  true.
