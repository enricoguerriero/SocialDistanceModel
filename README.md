# Social Distance Model

### Analisi dei microdati del 2020 della Banca d'Italia 

L'analisi che segue prevede la creazione di un modello efficiente in grado di stimare i consumi delle famiglie durante l'anno 2020.
Si ricerca la costruzione di un modello spaziale, basato sulle interazioni tra le osservazioni in funzione della loro distanza: in particolare, non si tratteranno distanza fisiche ma si ricercherà una "distanza sociale":
questa verrà definita come distanza euclidea (ma seguiranno tentativi anche con distanze di Manhattan e Gower) nel sottospazio vettoriale generato da alcune colonne del dataset. 
Cruciale per il modello sarà quindi la scelta delle colonne che verranno poi utilizzati per costruire tale distanza.

Il database utilizzato è quello fornito dalla Banca d'Italia (https://www.bancaditalia.it/statistiche/tematiche/indagini-famiglie-imprese/bilanci-famiglie/distribuzione-microdati/ricerca/ricerca.html?min_anno_pubblicazione=2023&max_anno_pubblicazione=2023).


## Il dataset

I dati forniti non sono racchiusi in un unico dataset, ma al contrario sono distribuiti in più tabelle, dalle quali è stata fatta una selezione:
si è scelto di operare con le tabelle Q20A, Q20B, Q20C1, Q20C2, Q20D, Q20E, Q20F e Q20G, contenenti le risposte di ciascuna famiglia ai questionari familiari, e aventi come chiave primaria la variabile NQUEST (Numero di questionario); è stata utilizzata anche un'ultima tabella, la tabella carcom20, contenente le risposte di tutti gli individui di ciascuna famiglia al questionario individuale, avente come chiavi primarie NQUEST e NORD (Numero d'ordine del componente all'interno della famiglia).
Alle tabelle sono state applicate aggregazioni e trasformazioni, al fine di ottenere un'unica tabella con le variabili di interesse per lo studio, aventi tutte come unità di riferimento la famiglia:
- NQUEST: Numero di questionario, è stata mantenuta solo perché chiave primaria anche di questa tabella.
- NCOMP: Numero di componenti della famiglia.
- VALOGG: Valore degli oggetti di lusso della famiglia.
- VALCAR: Valore delle macchine della famiglia.
- VALMEZ: Valore degli altri mezzi della famiglia.
- VALCA: Valore degli oggetti "della casa" della famiglia (es. elettrodomestici)
