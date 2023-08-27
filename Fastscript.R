# SOLO SCRIPT VELOCI DA ESEGUIRE COSI' POSSO RUNNARE TUTTO

# Download data
source("DataLibrary.R")

# Funzioni per matrice dei pesi

# Fisso il percentile
p <- 0.1

# Reciproco della distanza
w_list_create <- function(df) {
    # Trasformo il df in matrice delle distanze
    w_mat <- as.matrix(dist(df))
    # Mi prendo il valore del percentile
    qu <- quantile(w_mat, p)
    # Inverto la matrice delle distanze per ottenere una matrice di prossimità
    w_mat <- 1/w_mat
    # Faccio sì che tutte le osservazioni con distanza maggiore di qu abbiano prossimità 0
    # Distanza maggiore di qu vuol dire prossimità minore di 1/qu
    w_mat[w_mat < 1/qu] <- 0
    # La diagonale è 0 perché ogni osservazione non è vicina con se stessa
    diag(w_mat) <- 0
    w_mat[w_mat == Inf] <- max(w_mat[w_mat != Inf])
    # Individuo le righe composte solo da zeri (ISOLE)
    zero_rows <- apply(w_mat, 1, function(row) all(row == 0))
    # Le rimuovo dalla matrice
    w_mat <- w_mat[!zero_rows, !zero_rows]
    # Porto tutti i valori di vicinanza tra 0 e 1 con rescale
    w_mat <- apply(w_mat, 1, rescale)
    # Faccio sì che tutte le righe della matrice sommino a 1
    w_mat <- t(t(w_mat)/rowSums(w_mat))
    # Trasformo la matrice in una lista di pesi
    w_list <- mat2listw(w_mat, style = "W")
    # Fine!
    return(w_list)
}

# Lista dei pesi dicotomica
# Tutte le osservazioni entro un certo percentile sono prossime tra loro (prossimità 1) le altre no (prossimità 0)
# Poi standardizzata per righe
w_list_create_2 <- function(mydf) {
    # Trasformo il df in matrice delle distanze
    mat <- w_mat <- as.matrix(dist(mydf))
    # Mi prendo il valore del percentile
    qu <- quantile(w_mat, p)
    # Tutti i valori più vicini di questo percentile valgono 1
    w_mat[mat < qu] <- 1
    w_mat[mat > qu] <- 0
    # La diagonale è 0 perché ogni osservazione non è vicina con se stessa
    diag(w_mat) <- 0
    # Rimuovo tutte le ISOLE
    zero_rows <- apply(w_mat, 1, function(row) all(row == 0))
    w_mat <- w_mat[!zero_rows, !zero_rows]
    # Faccio sì che tutte le righe della matrice sommino a 1
    w_mat <- t(t(w_mat)/rowSums(w_mat))
    # Devo correggere gli na generati nelle righe con 0 interazioni
    mydf[is.na(mydf)] <- 0
    # Trasformo la matrice in una lista di pesi
    w_list <- mat2listw(w_mat, style = "W")
    # Fine!
    return(w_list)
}

zero_rows_finder <- function(mydf){
    # Trasformo il df in matrice delle distanze
    mat <- w_mat <- as.matrix(dist(mydf))
    # Mi prendo il valore del percentile
    qu <- quantile(w_mat, p)
    # Tutti i valori più vicini di questo percentile valgono 1
    w_mat[mat < qu] <- 1
    w_mat[mat > qu] <- 0
    # La diagonale è 0 perché ogni osservazione non è vicina con se stessa
    diag(w_mat) <- 0
    # Rimuovo tutte le ISOLE
    zero_rows <- apply(w_mat, 1, function(row) all(row == 0))
    w_mat <- w_mat[!zero_rows, !zero_rows]
    return(zero_rows)
}

# Creazione campione
set.seed(69)
# Il campione ha livello di confidenza 95% e margine di errore 5% (surveymonkey)
df_sample <- df %>% sample_n(size = 391)

# Creazione del modello a-spaziale
fit.sample <- lm(CONSUMO ~ STUDIO + VALUX + HOMEVAL + REDD, data = df_sample)
summary(fit.sample)

# Creazione delle due matrici dei pesi
wl1 <- w_list_create(df_sample[,c("STUDIO", "HOMEVAL", "VALUX", "REDD")])
wl2 <- w_list_create_2(df_sample[,c("STUDIO", "HOMEVAL", "VALUX", "REDD")])
# zero rows
zr <- zero_rows_finder(df_sample[,c("STUDIO", "VALUX", "HOMEVAL", "REDD")])

# Creo il modello con le sole variabili con pesi non nulli
fit.sample.w <- lm(CONSUMO ~ STUDIO + VALUX + HOMEVAL, data = df_sample[(zr == 0),])
summary(fit.sample.w)

# Test per vedere una possibile incidenza delle interazioni spaziali nel modello a-spaziale
# Confronto anche tra le due matrici dei pesi
# Metodo 1:
lm.LMtests(model = fit.sample.w, listw = wl1, test = "SARMA")
lm.LMtests(model = fit.sample.w, listw = wl1, test = "RLMerr")
lm.LMtests(model = fit.sample.w, listw = wl1, test = "RLMlag")
# Metodo 2:
lm.LMtests(model = fit.sample.w, listw = wl2, test = "SARMA")
lm.LMtests(model = fit.sample.w, listw = wl2, test = "RLMerr")
lm.LMtests(model = fit.sample.w, listw = wl2, test = "RLMlag")

# La presenza dell'interazione spaziale sembra essere abbastanza sicura

# Proviamo a costruire i primi modelli

sarar.fit.sample.1 <- sacsarlm(log(CONSUMO) ~ REDD + log(HOMEVAL) + log(VALUX) + STUDIO,
                               data = df_sample[(zr == 0),], listw = wl1)
sarar.fit.sample.2 <- sacsarlm(log(CONSUMO) ~ REDD + log(HOMEVAL) + log(VALUX) + STUDIO,
                             data = df_sample[(zr == 0),], listw = wl2)

summary(sarar.fit.sample.1)
summary(sarar.fit.sample.2)
# Risultati deludenti, i coefficienti di interazione spaziale non sono significativi

# Proviamo i modelli lag

sar.fit.sample.1 <- lagsarlm(log(CONSUMO) ~ REDD + log(HOMEVAL) + log(VALUX) + STUDIO,
                               data = df_sample[(zr == 0),], listw = wl1)
sar.fit.sample.2 <- lagsarlm(log(CONSUMO) ~ REDD + log(HOMEVAL) + log(VALUX) + STUDIO,
                               data = df_sample[(zr == 0),], listw = wl2)

summary(sar.fit.sample.1)
summary(sar.fit.sample.2)
# Anche in questo caso coefficiente non significativo, ma già meglio

# Proviamo i modelli error

sem.fit.sample.1 <- errorsarlm(log(CONSUMO) ~ REDD + log(HOMEVAL) + log(VALUX) + STUDIO,
                             data = df_sample[(zr == 0),], listw = wl1)
sem.fit.sample.2 <- errorsarlm(log(CONSUMO) ~ REDD + log(HOMEVAL) + log(VALUX) + STUDIO,
                             data = df_sample[(zr == 0),], listw = wl2)

summary(sem.fit.sample.1)
summary(sem.fit.sample.2)
# Risultati analoghi a modelli sar
