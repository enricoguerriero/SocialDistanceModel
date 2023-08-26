# SOLO SCRIPT VELOCI DA ESEGUIRE COSI' POSSO RUNNARE TUTTO

# Download data
source("DataLibrary.R")

# Funzioni per matrice dei pesi

w_list_create <- function(df) {
    # Trasformo il df in matrice delle distanze
    w_mat <- as.matrix(dist(df))
    # Mi prendo il valore del decimo percentile
    qu <- quantile(w_mat, 0.005)
    # Inverto la matrice delle distanze per ottenere una matrice di prossimità
    w_mat <- 1/w_mat
    # Faccio sì che tutte le osservazioni con distanza maggiore di qu abbiano prossimità 0
    # Distanza maggiore di qu vuol dire prossimità minore di 1/qu
    w_mat[w_mat < 1/qu] <- 0
    # La diagonale è 0 perché ogni osservazione non è vicina con se stessa
    diag(w_mat) <- 0
    # Porto tutti i valori di vicinanza tra 0 e 1 con rescale
    w_mat <- apply(w_mat, 1, rescale)
    # Faccio sì che tutte le righe della matrice sommino a 1
    w_mat <- t(t(w_mat)/rowSums(w_mat))
    # Trasformo la matrice in una lista di pesi
    w_list <- mat2listw(w_mat, style = "W")
    # Fine!
    return(w_list)
}
