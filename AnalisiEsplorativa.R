# Analisi esplorativa del dataset

# Import data
source("DataLibrary.R")


# Analisi univariata -----

# Barplots (variabili discrete)

# NCOMP
ncomp.plot <- ggplot(data = df, aes(x = NCOMP, fill = factor(NCOMP))) +
    geom_bar(col = "black") +
    theme_bw() +
    theme(legend.position = "") +
    labs(x = "Numero di componenti", y = "Frequenza") +
    scale_x_discrete(limits = factor(1:9)) 

# HAPPY
happy.plot <- ggplot(data = df, aes(x = HAPPY, fill = factor(HAPPY))) +
    geom_bar(col = "black") +
    theme_bw() +
    theme(legend.position = "") +
    labs(x = "Felicità", y = "Frequenza") +
    scale_x_discrete(limits = factor(1:10)) 

# STUDIO
studio.plot <- ggplot(data = df, aes(x = STUDIO, fill = factor(STUDIO))) +
    geom_bar(col = "black") +
    theme_bw() +
    theme(legend.position = "") +
    labs(x = "Titolo di studio", y = "Frequenza") +
    scale_x_discrete(limits = factor(1:8)) 

# OCC
occ.plot <- ggplot(data = df, aes(x = factor(OCC), fill = factor(OCC))) +
    geom_bar(col = "black") +
    theme_bw() +
    theme(legend.position = "") +
    labs(x = "Numero di occupati", y = "Frequenza") +
    scale_x_discrete(breaks = 0:6, limits = factor(0:6)) 

# DIP
dip.plot <- ggplot(data = df, aes(x = factor(DIP), fill = factor(DIP))) +
    geom_bar(col = "black") +
    theme_bw() +
    theme(legend.position = "") +
    labs(x = "Lavoratori dipendenti", y = "Frequenza") +
    scale_x_discrete(breaks = 0:5, limits = factor(0:5)) 

# IND
ind.plot <- ggplot(data = df, aes(x = factor(IND), fill = factor(IND))) +
    geom_bar(col = "black") +
    theme_bw() +
    theme(legend.position = "") +
    labs(x = "Lavoratori indipendenti", y = "Frequenza") +
    scale_x_discrete(breaks = 0:5, limits = factor(0:5)) 

# DIS
dis.plot <- ggplot(data = df, aes(x = factor(DIS), fill = factor(DIS))) +
    geom_bar(col = "black") +
    theme_bw() +
    theme(legend.position = "") +
    labs(x = "Numero di disoccupati", y = "Frequenza") +
    scale_x_discrete(breaks = 0:6, limits = factor(0:6)) 

# PENS
pens.plot <- ggplot(data = df, aes(x = factor(PENS), fill = factor(PENS))) +
    geom_bar(col = "black") +
    theme_bw() +
    theme(legend.position = "") +
    labs(x = "Numero di pensionati", y = "Frequenza") +
    scale_x_discrete(breaks = 0:3, limits = factor(0:3)) 

# FINMES
finmes.plot <- ggplot(data = df, aes(x = factor(FINMES), fill = factor(FINMES))) +
    geom_bar(col = "black") +
    theme_bw() +
    theme(legend.position = "") +
    labs(x = "Facilità arrivare a fine mese", y = "Frequenza") +
    scale_x_discrete(breaks = 1:6, limits = factor(1:6)) 

# Histograms (variabili continue)

# VALOGG
valogg.plot <- ggplot(data = df, aes(x = VALOGG)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore oggetti di valore", y = "Frequenza")
# Tolgo ultimi 5 percentili per visualizzare meglio distribuzione
valogg.plot.95 <- ggplot(data = df[df$VALOGG < quantile(df$VALOGG, probs = 0.95),], 
                         aes(x = VALCAR)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore oggetti di valore", y = "Frequenza")

# VALCAR
valcar.plot <- ggplot(data = df, aes(x = VALCAR)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore macchine", y = "Frequenza")
# Tolgo ultimi 5 percentili per visualizzare meglio distribuzione
valcar.plot.95 <- ggplot(data = df[df$VALCAR < quantile(df$VALCAR, probs = 0.95),], 
                         aes(x = VALCAR)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore macchine", y = "Frequenza")

# VALMEZ
valmez.plot <- ggplot(data = df, aes(x = VALMEZ)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore altri mezzi", y = "Frequenza")
# Tolgo ultimi 5 percentili per visualizzare meglio distribuzione
valmez.plot.95 <- ggplot(data = df[df$VALMEZ < quantile(df$VALMEZ, probs = 0.95),], 
                         aes(x = VALMEZ)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore altri mezzi", y = "Frequenza")

# VALCA
valca.plot <- ggplot(data = df, aes(x = VALCA)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore oggetti di casa", y = "Frequenza")
# Tolgo ultimi 5 percentili per visualizzare meglio distribuzione
valca.plot.95 <- ggplot(data = df[df$VALCA < quantile(df$VALCA, probs = 0.95),], 
                         aes(x = VALCA)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore oggetti di casa", y = "Frequenza")

# CIBINT
cibint.plot <- ggplot(data = df, aes(x = CIBINT)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore del cibo consumato a casa", y = "Frequenza")
cibint.plot.95 <- ggplot(data = df[df$CIBINT < quantile(df$CIBINT, probs = 0.95),]
                         , aes(x = CIBINT)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore del cibo consumato a casa", y = "Frequenza")

# CIBEST
cibest.plot <- ggplot(data = df, aes(x = CIBEST)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore del cibo consumato fuori casa", y = "Frequenza")
cibest.plot.95 <- ggplot(data = df[df$CIBEST < quantile(df$CIBEST, probs = 0.95),]
                         , aes(x = CIBEST)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore del cibo consumato fuori casa", y = "Frequenza")

# BOLLETTE
bollette.plot <- ggplot(data = df, aes(x = BOLLETTE)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Bollette", y = "Frequenza")
bollette.plot.95 <- ggplot(data = df[df$BOLLETTE < quantile(df$BOLLETTE, probs = 0.95),]
                         , aes(x = BOLLETTE)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Bollette", y = "Frequenza")

# ALTCONS
altcons.plot <- ggplot(data = df, aes(x = ALTCONS)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore degli altri consumi", y = "Frequenza")
# Tolgo ultimi 5 percentili per visualizzare meglio distribuzione
altcons.plot.95 <- ggplot(data = df[df$ALTCONS < quantile(df$ALTCONS, probs = 0.95),], 
                        aes(x = ALTCONS)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore degli altri consumi", y = "Frequenza")

# SOLDMENS
soldmens.plot <- ggplot(data = df, aes(x = SOLDMENS)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Soldi necessari al mese", y = "Frequenza")
soldmens.plot.98 <- ggplot(data = df[df$SOLDMENS < quantile(df$SOLDMENS, probs = 0.99) &
                                        df$SOLDMENS > quantile(df$SOLDMENS, probs = 0.01),], 
                          aes(x = SOLDMENS)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Soldi necessari al mese", y = "Frequenza")

# REDPERC
redperc.plot <- ggplot(data = df, aes(x = REDPERC)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Scostamento del reddito dalla media", y = "Frequenza")
# Tolgo primo e ultimo percentile per visualizzare meglio distribuzione
redperc.plot.98 <- ggplot(data = df[df$REDPERC < quantile(df$REDPERC, probs = 0.99) &
                                        df$REDPERC > quantile(df$REDPERC, probs = 0.01),], 
                          aes(x = REDPERC)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Scostamento del reddito dalla media", y = "Frequenza")

# VAROGG
varogg.plot <- ggplot(data = df, aes(x = VAROGG)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Variazione di valore degli oggetti (?)", y = "Frequenza")
# Anche togliendo percentili si osserva come la maggiorparte sia 0
# Distribuzione non migliorabile

# VARCAR
varcar.plot <- ggplot(data = df, aes(x = VARCAR)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Variazione di valore delle macchine", y = "Frequenza")
# Stesso discorso di varogg

# VARMEZ
varmez.plot <- ggplot(data = df, aes(x = VARMEZ)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Variazione di valore di altri mezzi", y = "Frequenza")
# Stesso discorso di varogg

# VARCA
varca.plot <- ggplot(data = df, aes(x = VARCA)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Variazione di valore di qualcosa (?)", y = "Frequenza")
# Stesso discorso di varogg

# VIAGGI
viaggi.plot <- ggplot(data = df, aes(x = VIAGGI)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Spese per viaggi nel 2020", y = "Frequenza")
# Tolgo ultimo percentile per visualizzare meglio distribuzione
viaggi.plot.99 <- ggplot(data = df[df$VIAGGI < quantile(df$VIAGGI, probs = 0.99),], 
                          aes(x = VIAGGI)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Spese per viaggi", y = "Frequenza")

# VARCON
varcon.plot <- ggplot(data = df, aes(x = VARCON)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Variazione di valore di consumi", y = "Frequenza")
# Stesso discorso di VAROGG
var0 <- sum(df$VARCON == 0)/length(df$VARCON)
# Quasi 84% non ha variato il proprio consumo

# VARCONAL
varconal.plot <- ggplot(data = df, aes(x = VARCONAL)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Variazione di valore di consumi alternativi (?)", y = "Frequenza")
# Vale sempre lo stesso discorso su quantili

# SEX
sex.plot <- ggplot(data = df, aes(x = SEX)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Proporzione di maschi", y = "Frequenza")

# VARISP
varisp.plot <- ggplot(data = df, aes(x = VARISP)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Variazione dei risparmi", y = "Frequenza")
# Solito discorso per quantili

# HOMEVAL
homeval.plot <- ggplot(data = df, aes(x = HOMEVAL)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore della casa", y = "Frequenza")

# VALUX
valux.plot <- ggplot(data = df, aes(x = VALUX)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore oggetti di lusso", y = "Frequenza")
# Tolgo ultimo percentile per visualizzare meglio distribuzione
valux.plot.99 <- ggplot(data = df[df$VALUX < quantile(df$VALUX, probs = 0.99),], 
                         aes(x = VALUX)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore oggetti di lusso", y = "Frequenza")

# CONSUMO
consumo.plot <- ggplot(data = df, aes(x = CONSUMO)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Valore del consumo nel 2020", y = "Frequenza")

# PROLAV
prolav.plot <- ggplot(data = df, aes(x = PROLAV)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Proporzione di lavoratori", y = "Frequenza")

# PROCIB
procib.plot <- ggplot(data = df, aes(x = PROCIB)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Proporzione di cibo consumato fuori casa", y = "Frequenza")

# PRORED
prored.plot <- ggplot(data = df, aes(x = PRORED)) +
    geom_histogram(bins = 20, col = "black", fill = "yellow") +
    theme_bw()  +
    labs(x = "Proporzione di percettori di reddito", y = "Frequenza")



# Analisi multivariata -----


# Vediamo le correlazioni
corplot <- ggcorrplot(cor(df))
# visualizzazione alternativa delle correlazioni
# argomenti eventuali: method = "ellipse" o method = "number"

# Troppo caotico, dividere il dataset in subset
df1 <- subset(df, select = c("CONSUMO", "NCOMP", "SEX", "STUDIO", "OCC", "DIP", 
                             "IND", "DIS", "PENS"))
corplot1 <- ggcorrplot(cor(df1))
cor1 <- cor(df$CONSUMO, df[,c("NCOMP", "SEX", "STUDIO", "OCC", "DIP", 
                              "IND", "DIS", "PENS")])
# subset 2
df2 <- subset(df, select = c("CONSUMO", "VALOGG", "VALMEZ", "VALCA", "VALUX", "HOMEVAL"))
corplot2 <- ggcorrplot(cor(df2))
cor2 <- cor(df$CONSUMO, df[,c("VALOGG", "VALMEZ", "VALCA", "VALUX", "HOMEVAL")])
# subset 3 (il meno sensato ma giusto per capire)
df3 <- subset(df, select = c("CONSUMO", "CIBINT", "CIBEST", "BOLLETTE", "ALTCONS", "VAROGG",
                             "VARCAR", "VARMEZ", "VIAGGI", "VARCONAL"))
corplot3 <- ggcorrplot(cor(df3))
cor3 <- cor(df$CONSUMO, df[,c("CIBINT", "CIBEST", "BOLLETTE", "ALTCONS", "VAROGG",
                              "VARCAR", "VARMEZ", "VIAGGI", "VARCONAL")])

# Vediamo gli scatterplot di tutte le variabili con il consumo
c.ncomp <- ggplot(data = df, aes(x = NCOMP, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.valogg <- ggplot(data = df, aes(x = VALOGG, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.valcar <- ggplot(data = df, aes(x = VALCAR, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.valmez <- ggplot(data = df, aes(x = VALMEZ, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.valca <- ggplot(data = df, aes(x = VALCA, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.cibint <- ggplot(data = df, aes(x = CIBINT, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.cibest <- ggplot(data = df, aes(x = CIBEST, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.bollette <- ggplot(data = df, aes(x = BOLLETTE, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.viaggi <- ggplot(data = df, aes(x = VIAGGI, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.altcons <- ggplot(data = df, aes(x = ALTCONS, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.soldmens <- ggplot(data = df, aes(x = SOLDMENS, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.finmes <- ggplot(data = df, aes(x = FINMES, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.happy <- ggplot(data = df, aes(x = HAPPY, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.homeval <- ggplot(data = df, aes(x = HOMEVAL, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.redperc <- ggplot(data = df, aes(x = REDPERC, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.redpercfine <- ggplot(data = df, aes(x = REDPERCFINE, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.varogg <- ggplot(data = df, aes(x = VAROGG, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.varcar <- ggplot(data = df, aes(x = VARCAR, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.varmez <- ggplot(data = df, aes(x = VARMEZ, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.varca <- ggplot(data = df, aes(x = VARCA, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.varcon <- ggplot(data = df, aes(x = VARCON, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.varconal <- ggplot(data = df, aes(x = VARCONAL, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.sex <- ggplot(data = df, aes(x = SEX, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.studio <- ggplot(data = df, aes(x = STUDIO, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.occ <- ggplot(data = df, aes(x = OCC, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.dip <- ggplot(data = df, aes(x = DIP, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.ind <- ggplot(data = df, aes(x = IND, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.pens <- ggplot(data = df, aes(x = PENS, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.dis <- ggplot(data = df, aes(x = DIS, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.prolav <- ggplot(data = df, aes(x = PROLAV, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.procib <- ggplot(data = df, aes(x = PROCIB, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.prored <- ggplot(data = df, aes(x = PRORED, y = CONSUMO)) +
    geom_point() +
    theme_bw() 
c.valux <- ggplot(data = df, aes(x = VALUX, y = CONSUMO)) +
    geom_point() +
    theme_bw() 

# Nell'analisi descrittiva guardo anche la distanza di mahalanobis
# Così, giusto perché si parla di distanza

# Non funziona perché il reciproco di alcuni valori è singolare
# Non era importante, lascio qui così in seguito eventualmente correggerò
mah_dist <- mahalanobis(df, center = colMeans(df), cov = cov(df))
plot_mahConsumo <- ggplot(df, aes(x = mah_dist, y = CONSUMO)) +
    geom_point(alpha = 0.7)  +
    xlab("Distanza di Mahalanobis dal centro") +
    ylab("Consumo") +
    ggtitle("Grafico di prova") +
    theme_bw()
