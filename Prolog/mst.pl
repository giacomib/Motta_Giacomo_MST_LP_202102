% Grillo Antonio 851970
% Motta Giacomo 851887

:- use_module(library(csv)).

:- dynamic graph/1.
:- dynamic arc/4.
:- dynamic vertex/2.
:- dynamic position/4.
:- dynamic new_arc/4.
:- dynamic heap_entry/4.
:- dynamic heap/2.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.


% LIBRERIA GRAFI

new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

delete_graph(G) :-
    retractall(arc(G, _, _, _)),
    retractall(vertex(G, _)),
    retractall(graph(G)),
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)),
    retractall(position(G, _, _, _)).

new_vertex(G, V) :- vertex(G, V), !.
new_vertex(G, V) :-
    new_graph(G),
    assert(vertex(G, V)),
    !.

graph_vertices(G, Vs) :-
    findall(vertex(G, V), vertex(G, V), Vs).

list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).

new_arc(G, U, V, Weight) :-
    arc(G, U, V, Weight),
    !.

new_arc(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    arc(G, U, V, _),
    retract(arc(G, U, V, _)),
    assert(arc(G, U, V, Weight)),
    !.

new_arc(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    arc(G, V, U, _),
    retract(arc(G, V, U, _)),
    assert(arc(G, U, V, Weight)),
    !.

new_arc(G, U, V, Weight) :-
    new_graph(G),
    new_vertex(G, U),
    new_vertex(G, V),
    assert(arc(G, U, V, Weight)),
    !.

new_arc(G, U, V) :- new_arc(G, U, V, 1).

graph_arcs(G, Es) :-
    var(Es),
    findall(arc(G, X, Y, Z), arc(G, X, Y, Z), Es),
    !.

graph_arcs(G, Es) :-
    length(Es, L1),
    findall(arc(G, _, _, _), arc(G, _, _, _), Xs),
    length(Xs, L2),
    L1 = L2,
    !,
    graph_arcs_ric(G, Es).

graph_arcs_ric(G, [E | Es]) :-
    E =.. [arc, G, S, D, W],
    findall(arc(G, S, D, W), arc(G, S, D, W), Xs),
    member(E, Xs),
    !,
    graph_arcs_ric(G, Es).

graph_arcs_ric(G, [E | Es]) :-
    E =.. [arc, G, S, D, W],
    findall(arc(G, S, D, W), arc(G, D, S, W), Xs),
    member(E, Xs),
    !,
    graph_arcs_ric(G, Es).

graph_arcs_ric(_, []) :-
    !.

vertex_neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), Ys),
    findall(arc(G, V, N, W), arc(G, N, V, W), Zs),
    append(Ys, Zs, Ns).

adjs(G, V, Vs) :-
    vertex(G, V),
    findall(vertex(G, A), arc(G, V, A, _), Ys),
    findall(vertex(G, B), arc(G, B, V, _), Zs),
    union(Ys, Zs, Vs).

list_arcs(G) :-
    graph(G),
    listing(arc(G, _, _, _)).

list_graph(G) :-
    graph(G),
    list_vertices(G),
    list_arcs(G).

read_graph(G, FileName) :-
    delete_graph(G),
    new_graph(G),
    csv_read_file(FileName, L, [separator(0'\t)]),
    insertInto(L, G).

insertInto([], _) :- !.

insertInto([L | Ls], G) :-
    L =.. [row | Xs],
    A =.. [new_arc, G | Xs],
    call(A),
    insertInto(Ls, G), !.

write_graph(G, FileName, graph) :-
    graph(G),
    findall(row(U, V, W), arc(G, U, V, W), L),
    csv_write_file(FileName, L, [separator(0'\t)]),
    !.

write_graph(G, FileName, edges) :-
    maplist(convert, G, Row),
    csv_write_file(FileName, Row, [separator(0'\t)]).

convert(P, L) :-
    P =.. [arc, _ | Ls],
    L =.. [row | Ls].

write_graph(G, FileName) :-
    write_graph(G, FileName, graph).

% MIN HEAP
new_heap(H) :- heap(H, _S), !.
new_heap(H) :- assert(heap(H, 0)), !.

delete_heap(H) :-
    retractall(heap(H, _)),
    retractall(heap_entry(H, _, _, _)).

heap_has_size(H, S) :- heap(H, S).

heap_empty(H) :- heap_has_size(H, 0).

heap_not_empty(H) :-
    heap_has_size(H, S),
    S \== 0.

swap(_, 0, _) :- !.

swap(_, _, 0) :- !.

swap(_, _, 1) :- !.

swap(H, P, M) :-
    retract(heap_entry(H, P, K1, V1)),
    retract(heap_entry(H, M, K2, V2)),
    assert(heap_entry(H, M, K1, V1)),
    assert(heap_entry(H, P, K2, V2)), !.

min_key(H, P1, P2, M) :-
    heap(H, _),
    heap_entry(H, P1, K1, _),
    heap_entry(H, P2, K2, _),
    K1 < K2,
    !,
    M = P1.

min_key(H, P1, P2, M) :-
    heap(H, _),
    heap_entry(H, P1, K1, _),
    heap_entry(H, P2, K2, _),
    K1 >= K2,
    !,
    M = P2.

heapify(H, _) :-
    heap_empty(H),
    !.

heapify(H, _) :-
    heap(H, 1),
    !.

heapify(H, P) :-
    heap(H, S),
    S < P,
    !.

heapify(H, P) :-
    heap(H, S),
    P =< S,
    L is P * 2,
    R is P * 2 + 1,
    L > S,
    R > S,
    !.

heapify(H, P) :-
    heap(H, S),
    P =< S,
    L is P * 2,
    R is P * 2 + 1,
    L =< S,
    R > S,
    heap_entry(H, L, _, _),
    heap_entry(H, P, _, _),
    min_key(H, P, L, M),
    P = M,
    !.

heapify(H, P) :-
    heap(H, S),
    P =< S,
    L is P * 2,
    R is P * 2 + 1,
    L =< S,
    R > S,
    heap_entry(H, L, _, _),
    heap_entry(H, P, _, _),
    min_key(H, P, L, M),
    P \= M,
    swap(H, P, M),
    heapify(H, M),
    !.

heapify(H, P) :-
    heap(H, S),
    P =< S,
    L is P * 2,
    R is P * 2 + 1,
    L =< S,
    R =< S,
    heap_entry(H, L, _, _),
    heap_entry(H, R, _, _),
    heap_entry(H, P, _, _),
    min_key(H, P, L, M1),
    min_key(H, M1, R, M),
    P = M,
    !.

heapify(H, P) :-
    heap(H, S),
    P =< S,
    L is P * 2,
    R is P * 2 + 1,
    L =< S,
    R =< S,
    heap_entry(H, L, _, _),
    heap_entry(H, R, _, _),
    heap_entry(H, P, _, _),
    min_key(H, P, L, M1),
    min_key(H, M1, R, M),
    P \= M,
    swap(H, P, M),
    heapify(H, M),
    !.

list_heap(H) :-
    listing(heap_entry(H, _, _, _)).

heap_head(H, K, V) :-
    heap_not_empty(H),
    !,
    heap_entry(H, 1, K, V).

parent(I, P) :-
    P is floor(I / 2).

heap_decrease_key(H, P) :-
    heap(H, _),
    P =< 1,
    !.

heap_decrease_key(H, P) :-
    heap(H, _),
    P > 1,
    parent(P, G),
    heap_entry(H, G, K1, _),
    heap_entry(H, P, K2, _),
    K1 =< K2,
    !.

heap_decrease_key(H, P) :-
    heap(H, _),
    P > 1,
    parent(P, G),
    heap_entry(H, G, K1, _),
    heap_entry(H, P, K2, _),
    K1 > K2,
    retract(heap_entry(H, G, K1, V1)),
    retract(heap_entry(H, P, K2, V2)),
    assert(heap_entry(H, P, K1, V1)),
    assert(heap_entry(H, G, K2, V2)),
    heap_decrease_key(H, G),
    !.

heap_insert(H, K, V) :-
    heap_has_size(H, S),
    S1 is S + 1,
    assert(heap_entry(H, S1, K, V)),
    retract(heap(H, S)),
    assert(heap(H, S1)),
    heap_decrease_key(H, S1).

heap_extract(H, K, V) :-
    heap(H, S),
    heap_not_empty(H),
    heap_head(H, K, V),
    swap(H, 1, S),
    retract(heap_entry(H, S, K, V)),
    S1 is S - 1,
    retract(heap(H, S)),
    assert(heap(H, S1)),
    heapify(H, 1), !.

modify_key(H, NewKey, OldKey, V) :-
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    heap_decrease_key(H, P),
    heap_entry(H, NP, NewKey, V),
    heapify(H, NP).

% MINIMUM SPANNING TREE: PRIM
mst_prim(G, Source) :-
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)),
    graph_vertices(G, Vs),
    init(G, Vs),
    retract(vertex_key(G, Source, _)),
    assert(vertex_key(G, Source, 0)),
    new_heap(q),
    make_heap(G, q, Vs),
    ciclo_heap(G, q),
    retractall(vertex_previous(_, _, nil)),
    !.

init(_G, []) :- !.

init(G, [I | Vs]) :-
    I =.. [vertex, G, V],
    assert(vertex_key(G, V, inf)),
    assert(vertex_previous(G, V, nil)),
    init(G, Vs).

make_heap(_, _, []) :- !.

make_heap(G, H, [I | Vs]) :-
    I =.. [vertex, G, V],
    vertex_key(G, V, W),
    heap_insert(H, W, V),
    make_heap(G, H, Vs).

ciclo_heap(_G, H) :-
    heap_empty(H), !.

ciclo_heap(G, H) :-
    heap_not_empty(H),
    heap_extract(H, _K, V),
    adjs(G, V, Vs),
    set_adjs(G, H, V, Vs),
    ciclo_heap(G, H).

set_adjs(_, _, _, []) :-
    !.

set_adjs(G, H, V, [A | As]) :-
    A =.. [vertex, G, N],
    findall(N, heap_entry(H, _, _, N), S),
    S = [],
    !,
    set_adjs(G, H, V, As).

set_adjs(G, H, V, [A | As]) :-
    A =.. [vertex, G, N],
    heap_entry(H, _, C, N),
    get_weight(G, V, N, W),
    vertex_key(G, N, K),
    K > W,
    !,
    retract(vertex_previous(G, N, _)),
    assert(vertex_previous(G, N, V)),
    retract(vertex_key(G, N, _)),
    assert(vertex_key(G, N, W)),
    modify_key(H, W, C, N),
    set_adjs(G, H, V, As).

set_adjs(G, H, V, [A | As]) :-
    A =.. [vertex, _, N],
    heap_entry(H, _, _, N),
    get_weight(G, V, N, W),
    vertex_key(G, N, K),
    W >= K,
    !,
    set_adjs(G, H, V, As).

get_weight(G, V, N, W) :-
    arc(G, V, N, W),
    !.

get_weight(G, V, N, W) :-
    arc(G, N, V, W),
    !.

mst_get(G, Source, PreorderTree) :-
    get_ordered_son(G, Source, SortedSon),
    set_son(G, SortedSon, PreorderTree),
    !.

set_son(G, As, Acc) :-
    length(As, L),
    L = 1,
    As =.. [arc, _, _, S, _],
    get_ordered_son(G, S, SS),
    append(As, [], Acc),
    set_son(G, SS, Acc),
    !.

set_son(_, [], []) :- !.

set_son(G, [A | As], Acc) :-
    A =.. [arc, _, _, S, _],
    get_ordered_son(G, S, SS),
    set_son(G, SS, Tmp),
    set_son(G, As, Tmp2),
    append([A], Tmp, Tmp3),
    append(Tmp3, Tmp2, Acc),
    !.

get_ordered_son(G, Source, Ord) :-
    findall(arc(G, Source, B, W),
	    (vertex_previous(G, B, Source),
	     vertex_key(G, B, W)),
	    Son),
    sort(3, @=<, Son, Sort1),
    sort(4, @=<, Sort1, Ord).
