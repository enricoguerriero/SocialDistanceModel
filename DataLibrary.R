# Script per importare le librerie e i dati utili per il progetto

# Librerie che potrebbero tornare utili
library(ggplot2)
library(forecast)
library(MASS)
library(ggfortify)
library(ggcorrplot)
library(dplyr)
library(stats)
library(sp)
library(spdep)
library(spatial)
library(spatialreg)
library(plot3D)
library(sphet)
library(scales)
library(cluster)

# Scarico tutti i database
Q20A  <- read.csv("data/q20a.csv")
Q20B <- read.csv("data/q20b.csv")
Q20C1 <- read.csv("data/q20c1.csv")
Q20C2  <- read.csv("data/q20c2.csv")
Q20D  <- read.csv("data/q20d.csv")
Q20E  <- read.csv("data/q20e.csv")
Q20F  <- read.csv("data/q20f.csv")
Q20G  <- read.csv("data/q20g.csv")

# Merge rispetto al numero del questionario
df2 <- merge(Q20A, Q20B, Q20C1, Q20C2, Q20D, Q20E, Q20F, Q20G, by = "NQUEST")

# Database riferito agli individui
df1  <- read.csv("C:/Users/enric/Desktop/Tesi/data/carcom20.csv")

# Creo un dataframe con le variabili di interesse
myframe <- df2[,c("NQUEST", "NCOMP", "VARRED", "VARREDA", "VARREDB", "VARREDFINE", "VARREDFINEA", "VARREDFINEB", "ACQUI1", "ACQUISA", "ACQUI21", "ACQUISB1",
                  "ACQUI22", "ACQUISB2", "ACQUI3", "ACQUISC", "VEND1", "VENDA", "VEND21", "VENDB2", "JWOVAT", "JWDURAT1A", "JWDURAT1B", "JWDURAT2", "JCONSALC2",
                  "JCONSALF2", "BOLLETTE", "VIAGGI", "VIAGGIT", "CONS2", "VARCONS", "VARCONSA", "VARCONSB", "VARCONSALIM", "VARCONSALIMA", "VARCONSALIMB",
                  "POVLIN", "CONDGEN", "HAPPY", "VALABIT")]

# Sistemo un po' il dataframe
# Creo una variabile che dice quanto le famiglie pensano che il proprio reddito si discosti dalla media
myframe$REDPERC <- rep(0,length(myframe$NQUEST))
for (i in seq_along(myframe$NQUEST)) {
  if (is.na(myframe$VARRED[i]) || myframe$VARRED[i] == 5) {
    myframe$REDPERC[i] <- NA
  } else {
    if (myframe$VARRED[i] == 2 || myframe$VARRED[i] == 4) {
      myframe$REDPERC[i] <- 0
    }
    if (myframe$VARRED[i] == 1) {
      myframe$REDPERC[i] <- myframe$VARREDA[i]
    }
    if (myframe$VARRED[i] == 3) {
      myframe$REDPERC[i] <- (-1) * myframe$VARREDB[i]
    }
  }
}
# Creo una variabile che indica quanto le famiglie pensano varierà il loro reddito nell'anno successivo
myframe$REDPERCFINE <- rep(0,length(myframe$NQUEST))
for (i in seq_along(myframe$NQUEST)) {
  if (is.na(myframe$VARREDFINE[i]) || myframe$VARREDFINE[i] == 5) {
    myframe$REDPERCFINE[i] <- NA
  } else {
    if (myframe$VARREDFINE[i] == 2 || myframe$VARREDFINE[i] == 4) {
      myframe$REDPERCFINE[i] <- 0
    }
    if (myframe$VARREDFINE[i] == 1) {
      myframe$REDPERCFINE[i] <- myframe$VARREDFINEA[i]
    }
    if (myframe$VARREDFINE[i] == 3) {
      myframe$REDPERCFINE[i] <- (-1) * myframe$VARREDFINEB[i]
    }
  }
}


# Creo una nuova variabile che rappresenta l'acquisto meno la vendita di oggetti preziosi
myframe$VAROGG <- rep(0,length(myframe$NQUEST))
for (i in 1:length(myframe$NQUEST)){
  if (is.na(myframe$ACQUI1[i]) || is.na(myframe$VEND1[i])){
    myframe$VAROGG[i] <- NA
  }
  else {
    if (myframe$ACQUI1[i] == 1) {
      myframe$VAROGG[i] <- myframe$VAROGG[i] + myframe$ACQUISA[i]
    }
    if (myframe$VEND1[i] == 1) {
      myframe$VAROGG[i] <- myframe$VAROGG[i] - myframe$VENDA[i] 
    }
  }
}

# Creo una nuova variabile che rappresenta l'acquisto meno la vendita di macchine
myframe$VARCAR <- rep(0,length(myframe$NQUEST))
for (i in 1:length(myframe$NQUEST)){
  if (is.na(myframe$ACQUI21[i]) || is.na(myframe$VEND21[i])){
    myframe$VARCAR[i] <- NA
  }
  else {
    if (myframe$ACQUI21[i] == 1) {
      myframe$VARCAR[i] <- myframe$VARCAR[i] + myframe$ACQUISB1[i]
    }
    if (myframe$VEND21[i] == 1) {
      myframe$VARCAR[i] <- myframe$VARCAR[i] - myframe$VENDB1[i] 
    }
  }
}

# Creo una nuova variabile che rappresenta l'acquisto meno la vendita di altri mezzi di trasporto
myframe$VARMEZ <- rep(0,length(myframe$NQUEST))
for (i in 1:length(myframe$NQUEST)){
  if (is.na(myframe$ACQUI22[i]) || is.na(myframe$VEND22[i])){
    myframe$VARMEZ[i] <- NA
  }
  else {
    if (myframe$ACQUI22[i] == 1) {
      myframe$VARMEZ[i] <- myframe$VARMEZ[i] + myframe$ACQUISB2[i]
    }
    if (myframe$VEND22[i] == 1) {
      myframe$VARMEZ[i] <- myframe$VARMEZ[i] - myframe$VENDB2[i] 
    }
  }
}

# Cambio i nomi di un po' di variabili
myframe$VARCA <- myframe$ACQUISC
myframe$VARCA[is.na(myframe$VARCA)] <- 0
names(myframe)[names(myframe) == "JWOVAT"] <- "VALOGG"
names(myframe)[names(myframe) == "JWDURAT1A"] <- "VALCAR"
names(myframe)[names(myframe) == "JWDURAT1B"] <- "VALMEZ"
names(myframe)[names(myframe) == "JWDURAT2"] <- "VALCA"
names(myframe)[names(myframe) == "JCONSALC2"] <- "CIBINT"
names(myframe)[names(myframe) == "JCONSALF2"] <- "CIBEST"
names(myframe)[names(myframe) == "v"] <- "VIAGGI"
names(myframe)[names(myframe) == "CONS2"] <- "ALTCONS"
names(myframe)[names(myframe) == "POVLIN"] <- "SOLDMENS"
names(myframe)[names(myframe) == "CONDGEN"] <- "FINMES"
names(myframe)[names(myframe) == "VALABIT"] <- "HOMEVAL"

# Faccio un'unica variabile con la variazione dei consumi
# NB questa variazione non comprende oggetti preziosi, auto, mezzi e oggetti per la casa
myframe$VARCON <- rep(0,length(myframe$NQUEST))
for (i in seq_along(myframe$NQUEST)) {
  if (is.na(myframe$VARCONS[i]) || myframe$VARCONS[i] == 5) {
    myframe$VARCON[i] <- NA
  } else {
    if (myframe$VARCONS[i] == 2 || myframe$VARCONS[i] == 4) {
      myframe$VARCON[i] <- 0
    }
    if (myframe$VARCONS[i] == 1) {
      myframe$VARCON[i] <- myframe$VARCONSA[i]
    }
    if (myframe$VARCONS[i] == 3) {
      myframe$VARCON[i] <- (-1) * myframe$VARCONSB[i]
    }
  }
}

# Faccio la stessa cosa per i consumi alimentari
myframe$VARCONAL <- rep(0,length(myframe$NQUEST))
for (i in seq_along(myframe$NQUEST)) {
  if (is.na(myframe$VARCONSALIM[i]) || myframe$VARCONSALIM[i] == 5) {
    myframe$VARCONAL[i] <- NA
  } else {
    if (myframe$VARCONSALIM[i] == 2 || myframe$VARCONSALIM[i] == 4) {
      myframe$VARCONAL[i] <- 0
    }
    if (myframe$VARCONSALIM[i] == 1) {
      myframe$VARCONAL[i] <- myframe$VARCONSALIMA[i]
    }
    if (myframe$VARCONSALIM[i] == 3) {
      myframe$VARCONAL[i] <- (-1) * myframe$VARCONSALIMB[i]
    }
  }
}

# Aggrego per famiglia i dati del df1 che sono personali
# La variabile sesso diventa la proporzione maschile nella famiglia
df1$SEX <- df1$SEX*(-1)+2
df1$B01 <- df1$B01*(-1)+2
agg_df <- aggregate(df1[,c("SEX")], by = list(df1$NQUEST), mean)
myframe$SEX <- agg_df$x
df1$DIP <- df1$APQUAL2 <= 5
df1$IND <- (df1$APQUAL2 >=6 & df1$APQUAL2 <= 10) | df1$APQUAL2 == 20
df1$DIS <- (df1$APQUAL2 >= 11 & df1$APQUAL2 <=14) | (df1$APQUAL2 >= 17 & df1$APQUAL2 <=19) | df1$APQUAL2 == 21
df1$PENS <- df1$APQUAL2 == 15 | df1$APQUAL2 == 16
# Il titolo di studio per una famiglia è il titolo più alto tra i componenti
agg_df <- aggregate(df1[,c("STUDIO")], by = list(df1$NQUEST), max)
myframe$STUDIO <- agg_df$x
agg_df <- aggregate(df1[,c("B01", "DIP", "IND", "DIS", "PENS")], by = list(df1$NQUEST), sum)
myframe$OCC <- agg_df$B01
myframe$DIP <- agg_df$DIP
myframe$IND <- agg_df$IND
myframe$DIS <- agg_df$DIS
myframe$PENS <- agg_df$PENS

df <- myframe
# Salvo le variabili che mi interessano per alleggerire il workspace
vars_to_keep <- c("df")
# lista di tutte le variabili nel workspace
all_vars <- ls()
# rimuovi tutte le variabili tranne quelle da mantenere
rm(list = setdiff(all_vars, vars_to_keep))
rm(all_vars)
