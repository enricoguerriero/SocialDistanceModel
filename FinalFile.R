
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

# Aggiungo i logaritmi
df$lCONSUMO <- log(df$CONSUMO)
df$lHOMEVAL <- log(df$HOMEVAL)
df$lVALUX <- log(df$VALUX)
df$lREDD <- log(df$REDD)
df[is.nan(df$lREDD),]$lREDD <- 0
df[df$lREDD == -Inf,]$lREDD <- 0


lfit <- lm(lCONSUMO ~ STUDIO + lVALUX + lHOMEVAL + lREDD + NCOMP, data = df)
summary(lfit)

# Grafici
r1 <- ggplot(data = df, mapping = aes(log(CONSUMO), resid(lfit))) +
    geom_point() +
    theme_bw() +
    labs(title = "Residui del modello", x = "Logaritmo del consumo", y = "Residui") +
    geom_hline(yintercept=0, linewidth = 1)
r2 <- ggplot(data = df, mapping = aes(resid(lfit))) +
    geom_histogram(aes(y =after_stat(density)),bins = 20,
                   fill = "yellow", alpha = 1, col = "black") + 
    geom_density(linewidth = 0.8, fill = "red", alpha = 0.3) +
    theme_bw() +
    labs(title = "Distribuzione dei residui", x = "Residui", y = "Densità")
r3 <- ggplot(data.frame(resid = resid(lfit)),aes(sample = resid)) + 
    stat_qq() +
    stat_qq_line(color = "red", linewidth = 1) +
    theme_bw() +
    labs(title = "q-q plot dei quantili dei residui", x = "Quantili empirici", y = "Quantili teorici")

ggsave("r1.png", plot = r1, width = 5, height = 4)
ggsave("r2.png", plot = r2, width = 5, height = 4)
ggsave("r3.png", plot = r3, width = 5, height = 4)

# Provo uno step aic
aicfit <- stepAIC(fit)
aiclfit <- stepAIC(lfit)

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

wl3 <- w_list_create(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])
zr <- zero_rows_finder(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])
# Solo 22  isole con questo metodo

# Modello senza isole a-spaziale
fit <- lm(CONSUMO ~ STUDIO + VALUX + HOMEVAL + REDD + NCOMP, data = df[zr==0,])
lfit <- lm(lCONSUMO ~ STUDIO + lVALUX + lHOMEVAL + lREDD + NCOMP, data = df[zr==0,])
summary(lfit)

# Test per valutare l'introduzione di un'interazione
# Confronto anche tra le due matrici dei pesi
# Metodo 1:
lm.LMtests(model = lfit, listw = wl3, test = "SARMA")
# p-value <2.2e-16
lm.LMtests(model = lfit, listw = wl3, test = "RLMerr")
# p-value 5.811e-8
lm.LMtests(model = lfit, listw = wl3, test = "RLMlag")
# p-value 6.728e-9

# L'abbassamento dei p-value è un risultato molto interessante

# Creazione del modello

sar.fit <- lagsarlm(lCONSUMO ~ lREDD + lHOMEVAL + lVALUX + STUDIO + NCOMP,
                    data = df[(zr == 0),], listw = wl3)
summary(sar.fit)
sem.fit <- errorsarlm(lCONSUMO ~ lREDD + lHOMEVAL + lVALUX + STUDIO + NCOMP,
                    data = df[(zr == 0),], listw = wl3)
summary(sem.fit)
sarar.fit <- sacsarlm(lCONSUMO ~ lREDD + lHOMEVAL + lVALUX + STUDIO + NCOMP,
                    data = df[(zr == 0),], listw = wl3)
summary(sarar.fit)

# Residui
rf1 <- ggplot(data = df[zr == 0,], mapping = aes(log(CONSUMO), resid(sar.fit))) +
    geom_point() +
    theme_classic() +
    labs(title = "Residui del modello", x = "Logaritmo del consumo", y = "Residui") +
    geom_hline(yintercept=0, linewidth = 1)
rf2 <- ggplot(data = df[zr == 0,], mapping = aes(resid(sar.fit))) +
    geom_histogram(aes(y =after_stat(density)),bins = 20,
                   fill = "yellow", alpha = 1, col = "black") + 
    geom_density(linewidth = 0.8, fill = "red", alpha = 0.3) +
    theme_classic() +
    labs(title = "Istogramma della distribuzione dei residui", x = "Residui", y = "Densità")
rf3 <- ggplot(data.frame(resid = resid(sar.fit)),aes(sample = resid)) + 
    stat_qq() +
    stat_qq_line(color = "red", linewidth = 1) +
    theme_classic() +
    labs(title = "Q-Q plot dei residui", x = "Quantili teorici", y = "Quantili empirici")
ggsave("rf1.png", plot = rf1, width = 5, height = 4)
ggsave("rf2.png", plot = rf2, width = 5, height = 4)
ggsave("rf3.png", plot = rf3, width = 5, height = 4)

# Test di Moran
moran.test(df[zr == 0,]$CONSUMO, wl1)
moran.test(df[zr == 0,]$CONSUMO, wl2)

# Test sui residui
moran.test(lfit$residuals, wl3)


moran.test(df[zr == 0,]$lCONSUMO, wl3)


simple.sar.fit <- lagsarlm(lCONSUMO ~ lREDD, data = df[(zr == 0),], listw = wl3)
summary(simple.sar.fit)

# Provo più cutoff diversi
p <- 0.01
wl3_0.01 <- w_list_create(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])
zr <- zero_rows_finder(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])

sar.fit_0.01 <- lagsarlm(lCONSUMO ~ lREDD + lHOMEVAL + lVALUX + STUDIO + NCOMP,
                         data = df[(zr == 0),], listw = wl3_0.01)
summary(sar.fit_0.01)

p <- 0.05
wl3_0.05 <- w_list_create(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])
zr <- zero_rows_finder(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])

sar.fit_0.05 <- lagsarlm(lCONSUMO ~ lREDD + lHOMEVAL + lVALUX + STUDIO + NCOMP,
                         data = df[(zr == 0),], listw = wl3_0.05)
summary(sar.fit_0.05)

p <- 0.15
wl3_0.15 <- w_list_create(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])
zr <- zero_rows_finder(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])

sar.fit_0.15 <- lagsarlm(lCONSUMO ~ lREDD + lHOMEVAL + lVALUX + STUDIO + NCOMP,
                         data = df[(zr == 0),], listw = wl3_0.15)
summary(sar.fit_0.15)

p <- 0.2
wl3_0.2 <- w_list_create(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])
zr <- zero_rows_finder(df[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])

sar.fit_0.2 <- lagsarlm(lCONSUMO ~ lREDD + lHOMEVAL + lVALUX + STUDIO + NCOMP,
                         data = df[(zr == 0),], listw = wl3_0.2)
summary(sar.fit_0.2)

# Inserisco in un df i dati per il grafico (a mano perché sto prima sì ok)

gdf <- data.frame(x = c(0.01, 0.05, 0.1, 0.15, 0.2), y = c(0.31069, 0.57133, 0.72418, 0.78566, 0.83343))

co <- ggplot(data = gdf, aes(x = x, y = y)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(x = "Cutoff sulla matrice delle prossimità", y = "valore del coefficiente spaziale")
ggsave("cutoff.png", plot = co, width = 5, height = 4)





# TRAIN TEST E TEST SET

# Faccio un train set di 70%
n <- dim(df)[1]/10*7
set.seed(69)
v <- sample(dim(df)[1], dim(df)[1]/10*7)
length(unique(v)) == length(v)
v <- sort(v$X1.dim.df..1.)
train.set <- df[v,c("lCONSUMO", "lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")]
test.set <- df[-v,c("lCONSUMO", "lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")]

p <- 0.1
wl3_tt <- w_list_create(train.set[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])
zr <- zero_rows_finder(train.set[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])

sar.fit_tt <- lagsarlm(lCONSUMO ~ lREDD + lHOMEVAL + lVALUX + STUDIO + NCOMP,
                         data = train.set[(zr == 0),], listw = wl3_tt)
summary(sar.fit_tt)

wl3_ttt <-  w_list_create(test.set[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])
zr <- zero_rows_finder(test.set[,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")])
pdf <- predict(sar.fit_tt, newdata = test.set[zr == 0,c("lHOMEVAL", "lVALUX", "lREDD", "NCOMP", "STUDIO")], 
        listw = wl3_ttt)
pdf <- as.numeric(pdf)
tt <- ggplot(data = test.set[zr == 0,], aes(x = lCONSUMO, y = lCONSUMO)) +
    geom_point(aes(x = test.set[zr == 0,]$lCONSUMO, y = pdf)) +
    theme_bw() +
    geom_abline(slope = 1, intercept = 0, col = "red", lwd = 1) +
    labs(x = "Valori reali", y = "Valori stimati dal modello",
         title = "Valori Reali vs Valori Stimati")
ggsave("tt.png", plot = tt, width = 5, height = 4)

tdf <- data.frame(a = test.set[zr == 0,]$lCONSUMO - pdf)
ttt <- ggplot(data =tdf, aes(x = 1:dim(tdf)[1],y = a)) +
    geom_point() +
    theme_bw() +
    geom_abline(slope = 0, intercept = 0, lwd = 1, col = "red") +
    labs(x = "Indice", y = "Differenza tra valori reali e valori stimati",
         title = "Distribuzione della Differenza tra Valori Reali e Stimati")
ggsave("ttt.png", plot = ttt, width = 5, height = 4)

t <- plot_grid(tt, ttt,
          nrow = 1)
ggsave("t.png", plot = t, width = 8, height = 4)



# moran plot a casissimo
mp <- moran.plot(df[zr == 0,]$lCONSUMO, listw = wl3,
           xlab = "Logaritmo del consumo", ylab = "Lag spaziale tra i consumi")
ggsave("mp.png", plot = mp, width = 5, height = 4)



# Provo a fare una heatmap
heatmap(listw2mat(wl3))


# Faccio un grafico per le slide se ci riesco

aaa <- data.frame(x = 0:4, y = 0:4)
b <- data.frame(x = c(1,2,1,4), y = c(3,3,2,1))
ex <- ggplot(aaa, aes(x,y)) +
    theme_bw() +
    geom_segment(aes(x = 1, y = 2, xend = 1, yend = 3), col = "red", lwd = 1) +
    geom_segment(aes(x = 1, y = 2, xend = 2, yend = 3), col = "red", lwd = 1) +
    geom_segment(aes(x = 1, y = 3, xend = 2, yend = 3), col = "red", lwd = 1) +
    geom_point(col = "white") +
    geom_point(data = b, aes(x, y), size = 3) +
    geom_text(aes(label = "Rook/Queen", x = 0.5, y = 2.5)) +
    geom_text(aes(label = "Rook/Queen", x = 1.5, y = 3.25)) +
    geom_text(aes(label = "Queen", x = 1.75, y = 2.25)) +
    geom_text(aes(label = "Isola", x = 3.75, y = 1)) 
ggsave("ex.png", plot = ex, width = 5, height = 4)
