La seguente libreria implementa più operazioni per la manipolazione di
grafi, MST (Minimum Spanning Tree) e MinHeap in linguaggio Common Lisp.
Con il carattere * indichiamo funzioni che sono state aggiunte da noi
per semplificare la complessità dei problemi e migliorare la leggibilità
del codice.

Funzioni implementate per i grafi:

is-graph graph-id → graph-id or NIL
Questa funzione ritorna il graph-id stesso se questo grafo è già stato
creato, oppure NIL se no.

new-graph graph-id → graph-id
Questa funzione genera un nuovo grafo e lo inserisce nel data base
(ovvero nella hash-table) dei grafi.

delete-graph graph-id → NIL
Rimuove l’intero grafo dal sistema (vertici archi etc); ovvero rimuove
tutte le istanze presenti nei data base (ovvero nelle hash-tables)
del sistema.

new-vertex graph-id vertex-id → vertex-rep
Aggiunge un nuovo vertice vertex-id al grafo graph-id.


new-neigh graph-id u v → neigh-list
Aggiunge, nella hashtable *neigh*, alla lista dei vicini di u il
vertice v. Viene effettuata la stessa cose per v.

graph-vertices graph-id → vertex-rep-list
Questa funzione torna una lista di vertici del grafo

new-arc graph-id vertex-id vertex-id &optional weight → arc-rep
Questa funzione aggiunge un arco del grafo graph-id nella
hash-table *arcs*.

graph-arcs graph-id → arc-rep-list
Questa funzione ritorna una lista di tutti gli archi presenti
in graph-id.

graph-vertex-neighbors graph-id vertex-id → arc-rep-list
Questa funzione ritorna una lista arc-rep-list contenente gli archi
che portano ai vertici N immediatamente raggiungibili da vertex-id.

graph-vertex-adjacent graph-id vertex-id → vertex-rep-list
Questa funzione ritorna una lista vertex-rep-list contenente i vertici
adiacenti a vertex-id.

graph-print graph-id
Questa funzione stampa alla console dell’interprete Common Lisp una
lista dei vertici e degli archi del grafo graph-id.



Funzioni implementate per i Minheap:

new-heap H &optional (capacity 42) → heap-rep
Questa funzione inserisce un nuovo heap nella hash-table *heaps*.

heap-delete heap-id → T
Questa funzione rimuove tutto lo heap indicizzato da heap-id.

heap-empty heap-id → boolean
Questa funzione è vera quando lo heap heap-id non contiene elementi.

heap-not-empty heap-id → boolean
Questa funzione è vera quando lo heap heap-id contiene almeno
un elemento.

heap-size heap-id → size
Questa funzione ritorna il numero di lementi presenti nello
heap heap-id.

*heap-actual-heap heap-id → heap
Questa funzione ritorna l'array contenente lo heap con il relativo heap-id.

*parent i → parent-index
Questa funzione ritorna l'indice del padre del nodo che si trova
all'indice i nell'array contenente lo heap.

*left i → left-son-index
Questa funzione ritorna l'indice del figlio sinistro del nodo
che si trova all'indice i nell'array contenente lo heap.

*right i → right-son-index
Questa funzione ritorna l'indice del figlio destro del nodo
che si trova all'indice i nell'array contenente lo heap.

*swap array i1 i2 → i1
Questa funzione scambia tra loro gli elementi nell'array contenente lo heap
che si trovano in posizione i1 e i2.

*heapify heap-id i → NIL
Questa funzione fa in modo che sia rispettata la proprietà del minheap,
ossia che la chiave di ogni nodo sia inferiore di quella dei propri figli.
Utilizza un approccio top-down. Ad esempio se la chiave è più grande dei figli, 
Effettua lo scambio, altrimenti resta invariata.

heap-head heap-id → (K V)
Questa funzione ritorna una lista di due elementi dove K è la chiave
minima e V il valore associato.

*heap-decrease-key heap-id i → NIL
Questa funzione fa in modo che sia rispettata la proprietà del minheap,
ossia che la chiave di ogni nodo sia inferiore di quella dei propri figli.
Utilizza un approccio bottom-up. Ad esempio se la chiave è più piccola del genitore,
Allora li scambia, altrimenti resta invariato.

heap-insert heap-id K V → boolean
Questa funzione inserisce l’elemento V nello heap heap-id con chiave K.

heap-extract heap-id → (K V)
Questa funzione ritorna la lista con K, V e con K minima;
la coppia è rimossa dallo heap heap-id.

heap-modify-key heap-id new-key old-key V → boolean
Questa funzione sostituisce la chiave OldKey (associata al valore V) con
NewKey.

heap-print heap-id → boolean
Questa funzione stampa sulla console lo stato interno dello heap heap-id



Funzioni implementate per i MST:

mst-vertex-key graph-id vertex-id → K
Questa funzione ritorna il peso minimo di un arco che connette
vertex-id nell’albero minimo.
Il vertice vertex-id deve appartenere al grafo graph-id.

mst-previous graph-id V → U
Questa funzione ritorna il vertice U che p il genitore del vertice
V nel MST.

*get-weight graph-id u v → W
Questa funzione ritorna il peso dell'arco che collega il vertice U
a V appartenenti al grafo graph-id.

*mst-recursion-heap heap-id graph-id → NIL
Questa funzione svuota lo heap heap-id  e costruisce il MST.

mst-prim graph-id source → NIL
Questa funzione costruisce le hash table necessarie per la costruzione
e gestione del MST.

*get-sons graph-id source → sons
Questa funzione ritorna la lista dei figli del nodo source ordinata
primariamente per peso degli archi ed a parità di peso, per ordine
lessicografico.

mst-get graph-id source → preorder-mst
Questa funzione ritorna preorder-mst che è una lista degli archi del MST
ordinata secondo un attraversamento preorder dello stesso, considerando
il nodo source come la radice dell'albero, fatta primariamente rispetto
al peso dell’arco ed a parità di peso, rispetto all'ordine lessicografico.
