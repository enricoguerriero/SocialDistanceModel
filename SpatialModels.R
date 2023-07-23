#
# COSTRUZIONE DEI MODELLI SPAZIALI
#

# Import dati
source("DataLibrary.R")

# Variabili che sembrano migliori candidate dall'analisi descrittiva:
# STUDIO
# PRORED
# VALUX
# HOMEVAL

# Intanto dichiaro una funzione che mi aiuterà a calcolare le weight matrix
# La funzione prende in input un dataframe e da in output la matrice dei pesi sotto forma di lista
w_list_create <- function(df) {
    # Trasformo il df in matrice delle distanze
    w_mat <- as.matrix(dist(df))
    # Mi prendo il valore del decimo percentile
    qu <- quantile(w_mat, 0.1)
    # Faccio sì che nessun osservazione sia lontana dalle altre più di questo percentile
    w_mat[w_mat > qu] <- qu
    # Creo una matrice delle vicinanze in questo modo
    # Quindi osservazioni che prima avevano distanza prossima a 0 ora sono vicine qu
    # Osservazioni di distanza maggiore o uguale a qu hanno vicinanza 0
    w_mat <- w_mat*(-1) + qu
    # La diagonale è 0 perché ogni osservazione non è vicina con se stessa
    diag(w_mat) <- 0
    # Porto tutti i valori di vicinanza tra 0 e 1 con rescale
    w_mat <- apply(w_mat, 1, rescale)
    # Trasformo la matrice in una lista di pesi
    w_list <- mat2listw(w_mat, style = "W")
    # Fine!
    return(w_list)
}
