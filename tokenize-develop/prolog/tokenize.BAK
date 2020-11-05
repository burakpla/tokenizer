:- module(tokenize,
          [ tokenize/2,
            tokenize/3,
            tokenize_file/2,
            tokenize_file/3,
            untokenize/2
          ]).



E.g.,

==
?- tokenize('Tokenizes: words,"strings", 1234.5\n', Tokens, [cased(true), spaces(false)]),
|    untokenize(Tokens, Codes).
Tokens = [word('Tokenizes'), punct(:), word(words), punct(','), string(strings), punct(','), number(1234.5), cntrl('\n')],
Codes = "Tokenizes:words,"strings"...34.5
".
==

`tokenize` is much more limited and much less performant than a lexer generator,
but it is dead simple to use and flexible enough for many common use cases.
*/

:- use_module(library(dcg/basics), [eos//0, number//1]).
:- use_module(tokenize_opts).


:- set_prolog_flag(back_quotes, codes).







tokenize(Text, Tokens) :-
    tokenize(Text, Tokens, []).





tokenize(Text, ProcessedTokens, Options) :-
    must_be(nonvar, Text),
    string_codes(Text, Codes),
    process_options(Options, PreOpts, TokenOpts, PostOpts),
    preprocess(PreOpts, Codes, ProcessedCodes),
    phrase(tokens(TokenOpts, Tokens), ProcessedCodes),
    postprocess(PostOpts, Tokens, ProcessedTokens),
    !.

non_tokens([T])    --> T.
non_tokens([T|Ts]) --> T, non_tokens(Ts).



tokenize_file(File, Tokens) :-
    tokenize_file(File, Tokens, []).



tokenize_file(File, Tokens, Options) :-
    read_file_to_codes(File, Codes, [encoding(utf8)]),
    tokenize(Codes, Tokens, Options).


untokenize(Tokens, Untokens) :-
    untokenize(Tokens, Untokens, []).
untokenize(Tokens, Untokens, _Options) :-
    maplist(token_to(codes), Tokens, TokenCodes),
    phrase(non_tokens(TokenCodes), Untokens),
    !.


preprocess(PreOpts, Codes, ProcessedCodes) :-
    preopts_data(cased, PreOpts, Cased),
    DCG_Rules = (
        preprocess_case(Cased)
    ),
    phrase(process_dcg_rules(DCG_Rules, ProcessedCodes), Codes).

postprocess(PostOpts, Tokens, ProcessedTokens) :-
    postopts_data(spaces, PostOpts, Spaces),
    postopts_data(cntrl, PostOpts, Cntrl),
    postopts_data(punct, PostOpts, Punct),
    postopts_data(to, PostOpts, To),
    postopts_data(pack, PostOpts, Pack),
    DCG_Rules = (
        keep_token(space(_), Spaces),
        keep_token(cntrl(_), Cntrl),
        keep_token(punct(_), Punct),
        convert_token(To)
    ),
    phrase(process_dcg_rules(DCG_Rules, PrePackedTokens), Tokens),
    (Pack
    -> phrase(pack_tokens(ProcessedTokens), PrePackedTokens)
    ;  ProcessedTokens = PrePackedTokens
    ).





process_dcg_rules(_, []) --> eos, !.
process_dcg_rules(DCG_Rules, []) --> DCG_Rules, eos, !.
process_dcg_rules(DCG_Rules, [C|Cs]) -->
    DCG_Rules,
    [C],
    process_dcg_rules(DCG_Rules, Cs).

preprocess_case(true), [C] --> [C].
preprocess_case(false), [CodeOut] --> [CodeIn],
    { to_lower(CodeIn, CodeOut) }.

keep_token(_, true), [T] --> [T].
keep_token(Token, false) --> [Token].
keep_token(Token, false), [T] --> [T], {T \= Token}.

convert_token(Type), [Converted] --> [Token],
    {token_to(Type, Token, Converted)}.

token_to(_, number(X), number(X)) :- !.
token_to(Type, Token, Converted) :-
    ( Type == strings -> Conversion = inverse(string_codes)
    ; Type == atoms   -> Conversion = inverse(atom_codes)
    ; Type == chars   -> Conversion = inverse(string_chars)
    ; Type == codes   -> Conversion = string_codes
    ),
    call_into_term(Conversion, Token, Converted).

pack_tokens([T])    --> pack_token(T).
pack_tokens([T|Ts]) --> pack_token(T), pack_tokens(Ts).

pack_token(P) --> pack(Token, N), {Token =.. [F,T], P =.. [F,T,N]}.

pack(X, Count) --> [X], pack(X, 1, Count).

pack(_, Total, Total)      --> eos.
pack(X, Total, Total), [Y] --> [Y], { Y \= X }.
pack(X, Count, Total)      --> [X], { succ(Count, NewCount) },
                               pack(X, NewCount, Total).



tokenize_text --> state(Text, Tokenized),
                  { phrase(tokens(Tokenized), Text) }.




tokens(Opts, [T])    --> token(Opts, T), eos, !.
tokens(Opts, [T|Ts]) --> token(Opts, T), tokens(Opts, Ts).




token(Opts, string(S)) -->
    { tokenopts_data(strings, Opts, true) },
    string(S).

token(Opts, number(N)) -->
    { tokenopts_data(numbers, Opts, true) },
    number(N), !.

token(_Opts, word(W))     --> word(W), eos, !.
token(_Opts, word(W)),` ` --> word(W), ` `.
token(_Opts, word(W)), C  --> word(W), (punct(C) ; cntrl(C) ; nasciis(C)).

token(_Opts, space(S))   --> space(S).
token(_Opts, punct(P)) --> punct(P).
token(_Opts, cntrl(C)) --> cntrl(C).
token(_Opts, other(O)) --> nasciis(O).

space(` `) --> ` `.

sep --> ' '.
sep --> eos, !.

word(W) --> csyms(W).

string(S) --> string(`"`, `"`, S).
string(OpenBracket, CloseBracket, S) --> string_start(OpenBracket, CloseBracket, S).


string_start(OpenBracket, CloseBracket, Cs) -->
    OpenBracket, string_content(OpenBracket, CloseBracket, Cs).


string_content(_OpenBracket, CloseBracket, []) --> CloseBracket, !.

string_content(OpenBracket, CloseBracket, [C|Cs]) -->
    escape, (CloseBracket | OpenBracket),
    {[C] = CloseBracket},
    string_content(OpenBracket, CloseBracket, Cs).

string_content(OpenBracket, CloseBracket, [C|Cs]) -->
    [C],
    {[C] \= CloseBracket},
    string_content(OpenBracket, CloseBracket, Cs).

csyms([L])    --> csym(L).
csyms([L|Ls]) --> csym(L), csyms(Ls).

csym(L)       --> [L], {code_type(L, csym)}.



nasciis([C])     --> nascii(C), eos, !.
nasciis([C]),[D] --> nascii(C), [D], {D < 127}.
nasciis([C|Cs])  --> nascii(C), nasciis(Cs).

nascii(C)        --> [C], {C > 127}.

' ' --> space.
' ' --> space, ' '.

escape --> `\\`.


... --> [].
... --> [_], ... .

space --> [S], {code_type(S, white)}.

punct([P]) --> [P], {code_type(P, punct)}.
cntrl([C]) --> [C], {code_type(C, cntrl)}.



codes_to_lower([], []).
codes_to_lower([U|Uppers], [L|Lowers]) :-
    code_type(U, to_upper(L)),
    codes_to_lower(Uppers, Lowers).

call_into_term(P, Term, Result) :-
    Term =.. [F, Arg],
    call(P, Arg, ResultArg),
    Result =.. [F, ResultArg].

inverse(P, A, B) :-
    call(P, B, A).

pad(T_Args, X, T_X_Args) :-
    T_Args   =.. [T|Args],
    T_X_Args =.. [T, X| Args].
