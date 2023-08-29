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
library(cowplot)
library(sf)
library(grid)
library(foreign)
library(readstata13)

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
df2 <- cbind(Q20A, Q20B, Q20C1, Q20C2, Q20D, Q20E, Q20F, Q20G)

# Database riferito agli individui
df1  <- read.csv("data/carcom20.csv")

# Creo un dataframe con le variabili di interesse
myframe <- df2[,c("NQUEST", "NCOMP", "VARRED", "VARREDA", "VARREDB", "VARREDFINE", "VARREDFINEA", "VARREDFINEB", "ACQUI1", "ACQUISA", "ACQUI21", "ACQUISB1",
                  "ACQUI22", "ACQUISB2", "ACQUI3", "ACQUISC", "VEND1", "VENDA", "VEND21", "VEND22", "VENDB1", "VENDB2", "JWOVAT", "JWDURAT1A", "JWDURAT1B", "JWDURAT2", "JCONSALC2",
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
# Rimuovo le colonne che non servono più
myframe <- subset(myframe, select=-c(VARRED, VARREDA, VARREDB))

# Creo una variabile che indica quanto le famiglie pensano varierÃ  il loro reddito nell'anno successivo
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
# Rimuovo le colonne che non servono più
myframe <- subset(myframe, select=-c(VARREDFINE, VARREDFINEA, VARREDFINEB))

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
# Rimuovo le colonne che non servono più
myframe <- subset(myframe, select=-c(ACQUI1, VEND1, ACQUISA, VENDA))

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
# Rimuovo le colonne che non servono più
myframe <- subset(myframe, select=-c(ACQUI21, ACQUISB1, VEND21, VENDB1))

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
# Rimuovo le colonne che non servono più
myframe <- subset(myframe, select=-c(ACQUI22, ACQUISB2, VEND22, VENDB2))

# Cambio i nomi di un po' di variabili
myframe$VARCA <- myframe$ACQUISC
myframe <- subset(myframe, select= -c(ACQUISC, ACQUI3))
myframe$VARCA[is.na(myframe$VARCA)] <- 0
myframe$VIAGGI <- myframe$VIAGGIT
myframe$VIAGGI[is.na(myframe$VIAGGI)] <- 0
myframe <- subset(myframe, select= -VIAGGIT)
names(myframe)[names(myframe) == "JWOVAT"] <- "VALOGG"
names(myframe)[names(myframe) == "JWDURAT1A"] <- "VALCAR"
names(myframe)[names(myframe) == "JWDURAT1B"] <- "VALMEZ"
names(myframe)[names(myframe) == "JWDURAT2"] <- "VALCA"
names(myframe)[names(myframe) == "JCONSALC2"] <- "CIBINT"
names(myframe)[names(myframe) == "JCONSALF2"] <- "CIBEST"
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
# Rimuovo le colonne che non servono più
myframe <- subset(myframe, select=-c(VARCONS, VARCONSA, VARCONSB))

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
# Rimuovo le colonne che non servono più
myframe <- subset(myframe, select=-c(VARCONSALIM, VARCONSALIMA, VARCONSALIMB))

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
# Il titolo di studio per una famiglia Ã¨ il titolo piÃ¹ alto tra i componenti
agg_df <- aggregate(df1[,c("STUDIO")], by = list(df1$NQUEST), max)
myframe$STUDIO <- agg_df$x
agg_df <- aggregate(df1[,c("B01", "DIP", "IND", "DIS", "PENS")], by = list(df1$NQUEST), sum)
myframe$OCC <- agg_df$B01
myframe$DIP <- agg_df$DIP
myframe$IND <- agg_df$IND
myframe$DIS <- agg_df$DIS
myframe$PENS <- agg_df$PENS


# Calcolo del reddito familiare


## read in family characteristics
carcom20 <- read.csv(file="data/carcom20.csv")

## extract chars of household head
capofam <- carcom20[carcom20$PARENT==1, ]

## age of household head
capofam$ETA <- 2020-capofam$ANASC
capofam$ETA2 <- capofam$ETA^2

## extract number of family members
numcompfam <- tapply(carcom20$NQUEST, carcom20$NQUEST, length)
numcompfam <- data.frame(NQUEST=as.numeric(names(numcompfam)),
                         numcompfam=numcompfam)

## stack different occupational forms
## read
for(i in 1:5) {
    istr <- paste("allb", i, " <- read.csv(file='data/allb", i, ".csv')", sep="")
    eval(parse(text=istr))
}

## by NQUEST, nord extract ym or similar

## dipendenti
a1 <- allb1[, c("NQUEST", "nord", "YLM")]
dimnames(a1)[[2]][3] <- "y1"
## autonomi
a2 <- allb2[, c("NQUEST", "nord", "YM", "YM2")]
a2$y2 <- a2$YM + a2$YM2
## atipici
a3 <- allb3[, c("NQUEST", "YM")]
dimnames(a3)[[2]][2] <- "y3"
## pensionati 
a4 <- allb4[, c("NQUEST", "nord", "TPENS", "MESIPEN")]
## somma su div+compfiss
a4$y4 <- a4$TPENS*a4$MESIPEN
## altre entrate
## (variabili che finiscono per "d"=dummy, per "v"=valore)
anames <- dimnames(allb5)[[2]]
allb5v <- allb5[, substr(anames, nchar(anames), nchar(anames))=="V"]
## sum all different types of income by column
y5 <- apply(allb5v, 1, sum, na.rm=TRUE)
a5 <- cbind(allb5[, c("NQUEST","nord")], y5)
rm(y5)

## merge all with 'nord'
a12 <- merge(a1, a2, all.x=T, all.y=T)
a123 <- merge(a12, a3, all.x=T, all.y=T)
a1234 <- merge(a123, a4, all.x=T, all.y=T)
a12345 <- merge(a1234, a5, all.x=T, all.y=T)
a.ind <- a12345[, c(1:3, 6:7, 10:11)]

## sum y by type for each ind
y.tot <- apply(a.ind[, -(1:2)], 1, sum, na.rm=TRUE)
a.ind$y.tot <- y.tot

## sum total y by family
y.fam <- tapply(a.ind$y.tot, a.ind$NQUEST, sum, na.rm=TRUE)


## calc. total y for household head
a.hh <- a.ind[a.ind$nord==1, ]
y.hh <- tapply(a.hh$y.tot, a.hh$NQUEST, sum, na.rm=TRUE)
## transf. into dataframes
dy.hh <- data.frame(NQUEST=as.numeric(names(y.hh)), y.hh=y.hh)
dy.fam <- data.frame(NQUEST=as.numeric(names(y.fam)), y.fam=y.fam)
ddy <- merge(dy.hh, dy.fam, by="NQUEST")

## calc. non-hh income
ddy$y.nhh <- ddy$y.fam-ddy$y.hh

# Unisco i data frame
myframe <- merge(myframe, ddy, by = "NQUEST")

myframe$REDD <- myframe$y.fam

# Salvo le variabili che mi interessano per alleggerire il workspace
vars_to_keep <- c("myframe")
# lista di tutte le variabili nel workspace
all_vars <- ls()
# rimuovi tutte le variabili tranne quelle da mantenere
rm(list = setdiff(all_vars, vars_to_keep))
rm(all_vars)

# Creo il dataframe senza gli NA 
df <-  myframe[complete.cases(myframe), ]
# In seguito considerazioni sulla rimozione dei dati

# Costruisco variabili che mi torneranno utili

# Creo la variabile valore degli oggetti di lusso
df$VALUX <- df$VALOGG + df$VALCAR + df$VALMEZ + df$VALCA

# Creo la variabile consumo
df$CONSUMO <- df$CIBINT + df$CIBEST + df$BOLLETTE/12 + df$VIAGGI/12 + df$ALTCONS

# Creo la variabile proporzione di lavoratori per nucleo
df$PROLAV <- df$OCC/df$NCOMP

# Creo la variabile proporzione di cibo consumata fuori casa
df$PROCIB <- df$CIBEST / (df$CIBEST + df$CIBINT)

# Creo la variabile proporzione di lavoratori e pensionati (percettori di soldi)
df$PRORED <- (df$OCC + df$PENS) / df$NCOMP

#df.sum <- summary(df)

