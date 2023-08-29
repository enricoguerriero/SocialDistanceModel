
################################

# SPERIAMO CHE SIA L'ULTIMO FILE

################################


# Obiettivo: far girare (anche molto lentamente) tutto il dataset e vedere 
# che risultati ottengo


# Import dei dati
source("DataLibrary.R")


# Funzioni per il calcolo della matrice dei pesi

# Prossimità come reciproco della distanza
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

# RICORDO CHE LE FUNZIONI HANNO BISOGNO DI UN PERCENTILE FISSATO PER GIRARE
# HA LO SCOPO DI ESSERE UTILIZZATO COME CUTOFF
# Per ora lo fisso io al 10 percentile, si può cambiare
p <- 0.1


# Costruzione del modello con le variabili che potrebbero essere significative
fit <- lm(CONSUMO ~ STUDIO + VALUX + HOMEVAL + REDD + NCOMP, data = df)
summary(fit)

lfit <- lm(lCONSUMO ~ STUDIO + lVALUX + lHOMEVAL + lREDD + NCOMP, data = df)
summary(lfit)

# Grafici
r1 <- ggplot(data = df, mapping = aes(log(CONSUMO), resid(lfit))) +
    geom_point() +
    theme_classic() +
    labs(title = "Residui del modello", x = "Logaritmo del consumo", y = "Residui") +
    geom_hline(yintercept=0, linewidth = 1)
r2 <- ggplot(data = df, mapping = aes(resid(lfit))) +
    geom_histogram(aes(y =after_stat(density)),bins = 20,
                   fill = "yellow", alpha = 1, col = "black") + 
    geom_density(linewidth = 0.8, fill = "red", alpha = 0.3) +
    theme_classic() +
    labs(title = "Distribuzione dei residui", x = "Residui", y = "Densità")
r3 <- ggplot(data.frame(resid = resid(lfit)),aes(sample = resid)) + 
    stat_qq() +
    stat_qq_line(color = "red", linewidth = 1) +
    theme_classic() +
    labs(title = "q-q plot dei quantili dei residui", x = "Quantili empirici", y = "Quantili teorici")

ggsave("r1.png", plot = r1, width = 5, height = 4)
ggsave("r2.png", plot = r2, width = 5, height = 4)
ggsave("r3.png", plot = r3, width = 5, height = 4)

# Provo uno step aic
aicfit <- stepAIC(fit)

# Costruzione della matrice di prossimità
wl1 <- w_list_create(df[,c("STUDIO", "HOMEVAL", "VALUX", "REDD", "NCOMP")])
wl2 <- w_list_create_2(df[,c("STUDIO", "HOMEVAL", "VALUX", "REDD", "NCOMP")])
# zero rows
zr <- zero_rows_finder(df[,c("STUDIO", "VALUX", "HOMEVAL", "REDD", "NCOMP")])

# Quante isole ci sono con cutoff 10%
sum(zr)
# 127 su più di 6000 dati è buono

# Modello senza isole a-spaziale
fit <- lm(CONSUMO ~ STUDIO + VALUX + HOMEVAL + REDD + NCOMP, data = df[zr==0,])
summary(fit)

# Test per valutare l'introduzione di un'interazione
# Confronto anche tra le due matrici dei pesi
# Metodo 1:
lm.LMtests(model = fit, listw = wl1, test = "SARMA")
# p-value 0.01029
lm.LMtests(model = fit, listw = wl1, test = "RLMerr")
# p-value 0.002652
lm.LMtests(model = fit, listw = wl1, test = "RLMlag")
# p-value 0.002797
# Metodo 2:
lm.LMtests(model = fit, listw = wl2, test = "SARMA")
# p-value 0.3378
lm.LMtests(model = fit, listw = wl2, test = "RLMerr")
# p-value 0.1407
lm.LMtests(model = fit, listw = wl2, test = "RLMlag")
# p-value 0.1677


# Costruzione dei modelli solo con la prima matrice dei pesi

sarar.fit <- sacsarlm(log(CONSUMO) ~ REDD + log(HOMEVAL) + log(VALUX) + STUDIO,
                               data = df[(zr == 0),], listw = wl1)
summary(sarar.fit)

sar.fit <- lagsarlm(log(CONSUMO) ~ REDD + log(HOMEVAL) + log(VALUX) + STUDIO,
                             data = df[(zr == 0),], listw = wl1)
summary(sar.fit)

sem.fit <- errorsarlm(log(CONSUMO) ~ REDD + log(HOMEVAL) + log(VALUX) + STUDIO,
                    data = df[(zr == 0),], listw = wl1)
summary(sem.fit)

# I modelli sembrano buoni, forse preferibile il modello con lag

# Visualizzazione grafica dei residui
ggplot(data = df[zr == 0,], mapping = aes(log(CONSUMO), resid(sarar.fit))) +
    geom_point() +
    theme_classic() +
    labs(title = "Residui del modello") +
    geom_hline(yintercept=0, linewidth = 1)
ggplot(data = df[zr == 0,], mapping = aes(log(CONSUMO), resid(sar.fit))) +
    geom_point() +
    theme_classic() +
    labs(title = "Residui del modello") +
    geom_hline(yintercept=0, linewidth = 1)
ggplot(data = df[zr == 0,], mapping = aes(log(CONSUMO), resid(sem.fit))) +
    geom_point() +
    theme_classic() +
    labs(title = "Residui del modello") +
    geom_hline(yintercept=0, linewidth = 1)

ggplot(data = df[zr == 0,], mapping = aes(resid(sarar.fit))) +
    geom_histogram(aes(y =after_stat(density)),bins = 20,
                   fill = "yellow", alpha = 1, col = "black") + 
    geom_density(linewidth = 0.8, fill = "red", alpha = 0.3) +
    theme_classic()
ggplot(data = df[zr == 0,], mapping = aes(resid(sar.fit))) +
    geom_histogram(aes(y =after_stat(density)),bins = 20,
                   fill = "yellow", alpha = 1, col = "black") + 
    geom_density(linewidth = 0.8, fill = "red", alpha = 0.3) +
    theme_classic()
ggplot(data = df[zr == 0,], mapping = aes(resid(sem.fit))) +
    geom_histogram(aes(y =after_stat(density)),bins = 20,
                   fill = "yellow", alpha = 1, col = "black") + 
    geom_density(linewidth = 0.8, fill = "red", alpha = 0.3) +
    theme_classic()

ggplot(data.frame(resid = resid(sarar.fit)),aes(sample = resid)) + 
    stat_qq() +
    stat_qq_line(color = "red", linewidth = 1) +
    theme_classic()
ggplot(data.frame(resid = resid(sar.fit)),aes(sample = resid)) + 
    stat_qq() +
    stat_qq_line(color = "red", linewidth = 1) +
    theme_classic()
ggplot(data.frame(resid = resid(sem.fit)),aes(sample = resid)) + 
    stat_qq() +
    stat_qq_line(color = "red", linewidth = 1) +
    theme_classic()

# Residui visibilmente problematici per presenza di trend (lineare)

# Moran's plot
moran.plot(df[zr == 0,]$CONSUMO, listw = wl1)


# Nuova matrice dei pesi:
# Costruisco i logaritmi delle variabili e le utilizzo sotto logaritmo per la matrice
df$lCONSUMO <- log(df$CONSUMO)
df$lHOMEVAL <- log(df$HOMEVAL)
df$lVALUX <- log(df$VALUX)
df$lREDD <- log(df$REDD)
df[is.nan(df$lREDD),]$lREDD <- 0
df[df$lREDD == -Inf,]$lREDD <- 0

wl3 <- w_list_create(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])
zr <- zero_rows_finder(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])
# Solo 22  isole con questo metodo

# Modello senza isole a-spaziale
fit <- lm(CONSUMO ~ STUDIO + VALUX + HOMEVAL + REDD + NCOMP, data = df[zr==0,])
lfit <- lm(lCONSUMO ~ STUDIO + lVALUX + lHOMEVAL + lREDD + NCOMP, data = df[zr==0,])
summary(fit)

# Test per valutare l'introduzione di un'interazione
# Confronto anche tra le due matrici dei pesi
# Metodo 1:
lm.LMtests(model = fit, listw = wl3, test = "SARMA")
# p-value <2.2e-16
lm.LMtests(model = fit, listw = wl3, test = "RLMerr")
# p-value 5.811e-8
lm.LMtests(model = fit, listw = wl3, test = "RLMlag")
# p-value 6.728e-9

# L'abbassamento dei p-value è un risultato molto interessante

# Creazione del modello

sar.fit <- lagsarlm(log(CONSUMO) ~ REDD + log(HOMEVAL) + log(VALUX) + STUDIO,
                    data = df[(zr == 0),], listw = wl3)
summary(sar.fit)

# Residui
ggplot(data = df[zr == 0,], mapping = aes(log(CONSUMO), resid(sar.fit))) +
    geom_point() +
    theme_classic() +
    labs(title = "Residui del modello") +
    geom_hline(yintercept=0, linewidth = 1)


# Test di Moran
moran.test(df[zr == 0,]$CONSUMO, wl1)
moran.test(df[zr == 0,]$CONSUMO, wl2)
