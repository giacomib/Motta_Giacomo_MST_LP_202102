%%% -*- Mode: Text -*-


La seguente libreria implementa più operazioni per la manipolazione di
grafi, MST (Minimum Spanning Tree) e MinHeap in linguaggio Prolog.
Con il carattere * indichiamo predicati che sono stati aggiunti da noi
per semplificare la complessità dei problemi e migliorare la leggibilità
del codice.


Predicati implementati per i grafi:

new_graph(G).
Questo predicato inserisce un nuovo grafo, di nome G, nella base-dati
Prolog.

delete_graph(G).
Questo predicato rimuove interamente il grafo, quindi anche i suoi archi
e vertici, dalla base-dati Prolog.

new_vertex(G, V).
Questo predicato aggiunge il vertice V alla base-dati Prolog.

graph_vertices(G, Vs).
Questo predicato è vero quando Vs è una lista contenente tutti i vertici
del grafo G.

list_vertices(G).
Questo predicato ritorna alla console dell'interprete Prolog una lista
dei vertici del grafo G.

new_arc(G, U, V, Weight).
Questo predicato aggiunge un arco del grafo G alla base-dati Prolog.

graph_arcs(G, Es).
Questo predicato è vero quando Es è una lista di tutti gli archi
presenti in G.

*graph_arcs_ric(G, Es).
Questo predicato è vero quando Es è una lista di archi presenti in G.
Inoltre garantisce la bidirezionalità degli archi nella lista Es. 

vertex_neighbors(G, V, Ns).
Questo predicato è vero quando V è un vertice del grafo G e Ns è una
lista contenente tutti gli archi, in formato arc(G, V, N, W), che sono
direttamente raggiungibili da V.

adjs(G, V, Vs).
Questo predicato è vero quando V è un vertice di G e Vs è una lista di
vertici, in formato vertex(G, V), adiacenti al vertice V.

list_arcs(G).
Questo predicato ritorna alla console dell'interprete Prolog una lista
degli archi del grafo G.

list_graph(G).
Questo predicato ritorna alla console dell'interprete Prolog una lista
dei vertici del grafo G.

read_graph(G, FileName).
Questo predicato legge un grafo G, da un file Filename di tipo csv e lo inserisce
nella base-dati Prolog. Il formato del file deve essere il seguente:
ogni riga contiene 3 elementi separati da un carattere di tabulazione,
i 3 elementi sono rispettivamente vertice di partenza, vertice di arrivo,
peso arco.
Se si vuole inserire un numero non intero, come sepatarore della parte
decimale si deve usare il punto.


insertInto(Ls, G).
Questo predicato accetta in input la lista dei predicati row(_, _, _)
letti da read_graph, li converte in realitivi new_arc e chiama
quest'ultimo. 

write_graph(G, FileName).
Questo predicato è vero quando G viene scritto sul file FileName,
G è un termine che identifica un grafo nella base-dati Prolog;
In FileName saranno scritti gli archi del grafo secondo il formato
descritto per read_graph/2.

write_graph(G, FileName, Type).
Questo predicato è vero quando G viene scritto sul file FileName,
se Type è graph, allora G è un termine che identifica un grafo
nella base-dati Prolog; In FileName saranno scritti gli archi del
grafo secondo il formato descritto per read_graph/2.
Se Type è edges, allora G è una lista di archi, ognuno dei quali viene
stampato su fileName, secondo il formato descritto per read_graph/2.

*convert(P, L).
Questo predicato è vero quando P è un arco presente nella
base-dati Prolog e L è una conversione in tipo row della rappresentazione dell'arco,
il predicato è compatibile con la sintassi di csv_write_file/3.



Predicati implementati per i Minheap:

new_heap(H).
Questo predicato inserisce un nuovo heap H nella base-dati Prolog.

delete_heap(H).
Questo predicato rimuove tutto lo heap dalla base-dati Prolog.

heap_has_size(H, S).
Questo predicato è vero quando S è la dimensione corrente dello heap H.

heap_empty(H).
Questo predicato è vero quando lo heap H non contiene elementi.

heap_not_empty(H).
Questo predicato è vero quando lo heap H contiene almeno un elemento.

*swap(H, P, M).
Questo predicato scambia tra loro gli elementi P ed M che appartengono
allo heap H. Esso apporta modifiche alla base dati.

*min_key(H, P1, P2, M).
Questo predicato è vero quando M è la posizione con chiave minore tra le 
chiavi alle posizioni dello heap P1 e P2.

*heapify(H, P).
Questo predicato fa in modo che sia rispettata la proprietà del minheap,
ossia che la chiave di ogni nodo sia inferiore di quella dei propri figli.
Utilizza un approccio top-down.

list_heap(H).
Questo predicato stampa sulla console Prolog lo stato interno dello heap.

heap_head(H, K, V).
Questo predicato è vero quando l'elemento dello heap h, con chiave
minima K, è V.

heap_insert(H, K, V).
Questo predicato è vero quando l'elemento V è inserito nello heap H
con chiave K.

heap_extract(H, K, V).
Questo predicato è vero quando la coppia K, V, con K minima, è rimossa
dallo heap.

parent(I, P).
Vero se P è il genitore indice di I nello heap.

modify_key(H, NewKey, oldKey, V).
Questo predicato p vero quando la chiave OldKey, associata al valore V,
è sostituita da NewKey; Il predicato è sviluppato in maniera tale da
mantenere la proprietà del min heap.


Predicati implementati per i MST:

vertex_key(G, V, K).
Questo predicato è vero quando V è un vertice di G e K contiene il peso
minimo di un arco che connette V nell'alebero minimo.

vertex_previous(G, V, U).
Questo predicato è vero quando V e U sono vertici di G e il vertice U
è il precedente di V nel MST.

mst_prim(G, Source).
Questo predicato ha successo con un effetto collaterale.
Dopo la sua prova, la base-dati Prolog ha al suo interno i predicati
vertex_key(G, V, K) per ogni V appartenente a G; la base-dati contiene
anche i predicati vertex_previous(G, V, U) per ogni V, ottenuti durante
le iterazioni dell'algoritmo di Prim.

*init(G, Is).
Questo predicato inizializza il MST nella base-dati Prolog tramite i
predicati vertex_key/3 e vertex_previous/3, ponendo il peso degli archi
a inf e i precedenti a nil.

*make_heap(G, H, Is).
Questo predicato associa inizializza lo heap con l'inserimento dei vertici
E delle loro chiavi iniziale.

*ciclo_heap(G, H).
Questo costruisce il MST effettuando una ricorsione sullo heap H finché non 
è vuoto. Esso estrarrà il minimo e imposterà i suoi adiacenti aggiornano lo heap
E I predicati vertex_key/3 e vertex_previous/3.

*set_adjs(G, H, V, As).
Questo predicato imposta la corretta vertex_key/3 e vertex_previous/3 della lista As 
degli adiacenti del vertice V mantenendo la coerenza con lo heap e MST.

*get_weight(G, V, N, W).
Questo predicato è vero quando W è il peso dell'arco che collega il nodo
V al nodo N.

mst_get(G, Source, PreorderTree).
Questo predicato è vero quando PreorderTree è una lista degli archi del
MST ordinata secondo un attraversamento preorder dello stesso, fatta
rispetto al peso degli archi, e a parità di peso, vengono attraversati
secondo l'ordinamento lessicografico.

*set_son(G, As, Acc).
Questo predicato è vero quando Acc è una lista contenente tutti i sottofigli
dei nodi presenti nella lista As, ordinati rispetto al peso degli archi,
e a parità di peso, secondo l'ordinamento lessicografico.

*get_ordered_son(G, Source, Ord).
Questo predicato è vero quando Ord è una lista di archi che vanno dal nodo
Source verso i figli dello stesso, che si trovano nel primo livello sottostante 
a Source dell'MST, ordinati rispetto al peso degli archi,
e a parità di peso, secondo l'ordinamento lessicografico.








