:- module(comment,
          [comment//2,
           comment_rec//2,
           comment_token//3,
           comment_token_rec//3]).


see:

  @see tests/test_comments.pl

The matchers predicates exported and defined are:

 comment(+Start:2matcher,+End:2matcher)
   - anonymously match a non recursive comment

 comment_rec(+Start:2matcher,+End:2matcher,2matcher)
   - anonymously match a recursive comment

 coment_token(+Start:2matcher,+End:2matcher,-Matched:list(codes))
   - match an unrecursive comment outputs the matched sequence used
     for building a resulting comment token

 coment_token_rec(+Start:2matcher,+End:2matcher,-Matched:list(codes))
   - match an recursive comment outputs the matched sequence used
     for building a resulting comment token
*/



comment_body(E) --> call(E),!.
comment_body(E) --> [_],comment_body(E).

comment(S,E) -->
    call(S),
    comment_body(E).


comment_body_token(E,Text) -->
    call(E,HE),!,
    {append(HE,[],Text)}.

comment_body_token(E,[X|L]) -->
    [X],
    comment_body_token(E,L).

comment_token(S,E,Text) -->
    call(S,HS),
    {append(HS,T,Text)},
    comment_body_token(E,T).


comment_body_rec_start(_,_,[]).

comment_body_token_rec(_,E,Cont,Text) -->
    call(E,HE),!,
    {append(HE,T,Text)},
    call(Cont,T).

comment_body_token_rec(S,E,Cont,Text) -->
    call(S,HS),!,
    {append(HS,T,Text)},
    comment_body_token_rec(S,E,comment_body_token_rec(S,E,Cont),T).

comment_body_token_rec(S,E,Cont,[X|L]) -->
    [X],
    comment_body_token_rec(S,E,Cont,L).

comment_token_rec(S,E,Text) -->
    call(S,HS),
    {append(HS,T,Text)},
    comment_body_token_rec(S,E,comment_body_rec_start,T).


comment_body_rec(_,E) -->
    call(E),!.

comment_body_rec(S,E) -->
    call(S),!,
    comment_body_rec(S,E),
    comment_body_rec(S,E).

comment_body_rec(S,E) -->
    [_],
    comment_body_rec(S,E).

comment_rec(S,E) -->
    call(S),
    comment_body_rec(S,E).
