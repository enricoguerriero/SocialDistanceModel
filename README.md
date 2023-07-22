# Social Distance Model

### Analisi dei microdati del 2020 della Banca d'Italia 

L'analisi che segue prevede la creazione di un modello efficiente in grado di stimare i consumi delle famiglie durante l'anno 2020.
Si ricerca la costruzione di un modello spaziale, basato sulle interazioni tra le osservazioni in funzione della loro distanza: in particolare, non si tratteranno distanza fisiche ma si ricercherà una "distanza sociale":
questa verrà definita come distanza euclidea (ma seguiranno tentativi anche con distanze di Manhattan e Gower) nel sottospazio vettoriale generato da alcune colonne del dataset. 
Cruciale per il modello sarà quindi la scelta delle colonne che verranno poi utilizzati per costruire tale distanza.

Il database utilizzato è quello fornito dalla Banca d'Italia.
