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

# Intanto dichiaro una funzione che mi aiuter� a calcolare le weight matrix
# La funzione prende in input un dataframe e da in output la matrice dei pesi sotto forma di lista
w_list_create <- function(df) {
    # Trasformo il df in matrice delle distanze
    w_mat <- as.matrix(dist(df))
    # Mi prendo il valore del decimo percentile
    qu <- quantile(w_mat, 0.05)
    # Faccio s� che nessun osservazione sia lontana dalle altre pi� di questo percentile
    w_mat[w_mat > qu] <- qu
    # Creo una matrice delle vicinanze in questo modo
    # Quindi osservazioni che prima avevano distanza prossima a 0 ora sono vicine qu
    # Osservazioni di distanza maggiore o uguale a qu hanno vicinanza 0
    w_mat <- w_mat*(-1) + qu
    # La diagonale � 0 perch� ogni osservazione non � vicina con se stessa
    diag(w_mat) <- 0
    # Porto tutti i valori di vicinanza tra 0 e 1 con rescale
    w_mat <- apply(w_mat, 1, rescale)
    # Trasformo la matrice in una lista di pesi
    w_list <- mat2listw(w_mat, style = "W")
    # Fine!
    return(w_list)
}



# Procedimento per passi secondo quanto appreso al colloquio:


# 1: Test spaziale su y: esiste correlazione nella variabile dipendente?

# Visualizzo la distribuzione della variabile risposta
r1 <- ggplot(data = df, aes(y = CONSUMO)) +
    geom_boxplot(fill = "yellow") +
    theme_bw() +
    labs(y = "CONSUMO") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    xlim(-0.8,0.8) +
    geom_segment(data  = df, aes(x = -0.375, xend = 0.375, y = mean(CONSUMO), yend = mean(CONSUMO)),
                 linewidth = 1, linetype = "dashed") +
    xlab("")+
    geom_text(data = df, aes(x = 0.4, y = mean(CONSUMO), label = sprintf("Media")),
              hjust = 0, vjust = 0) +
    geom_text(data = df, aes(x = 0.4, y = median(CONSUMO) - 400,
                               label = sprintf("Mediana")),
              hjust = 0, vjust = 0)
r2 <- ggplot(data = df, aes(x = CONSUMO)) +
    geom_histogram(aes(y = after_stat(density)), col = "black", fill = "yellow", bins = 20) +
    geom_density(linewidth = 0.8, fill = "pink", alpha = 0.3) +
    theme_bw() +
    labs(y = "CONSUMO", x = "") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
title <- ggdraw() + 
    draw_label(
        "Distribuzione del consumo",
        fontface = 'bold',
        x = 0,
        hjust = 0
    ) +
    theme(
        plot.margin = margin(0, 0, 0, 7)
    )
rowsplot <- plot_grid(r1, r2,
                      nrow = 1)
plot_grid(title,
          rowsplot,
          ncol = 1,
          rel_heights = c(0.1, 1))

# Breve analisi delle esplicative selezionate e il consumo
plot <- ggplot() +
    theme_void()
riquadro <- rectGrob(gp = gpar(fill = "white", col = "black"))
plot <- plot + annotation_custom(riquadro, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
title1 <- plot + annotation_custom(ggplotGrob(
    ggdraw() +
        draw_label(
            "CONSUMO",
            fontface = 'bold',
            x = 0.5,
            y = 0.5,
            hjust = 0.5,
            vjust = 0.5,
            size = 20
        )
), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
title2 <- plot + annotation_custom(ggplotGrob(
    ggdraw() +
        draw_label(
            "TITOLO DI STUDIO",
            fontface = 'bold',
            x = 0.5,
            y = 0.5,
            hjust = 0.5,
            vjust = 0.5,
            size = 20
        )
), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
title3 <- plot + annotation_custom(ggplotGrob(
    ggdraw() +
        draw_label(
            "VALORE DELLA CASA",
            fontface = 'bold',
            x = 0.5,
            y = 0.5,
            hjust = 0.5,
            vjust = 0.5,
            size = 20
        )
), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
title4 <- plot + annotation_custom(ggplotGrob(
    ggdraw() +
        draw_label(
            "OGGETTI DI LUSSO",
            fontface = 'bold',
            x = 0.5,
            y = 0.5,
            hjust = 0.5,
            vjust = 0.5,
            size = 20
        )
), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
cor1 <- plot + annotation_custom(ggplotGrob(
    ggdraw() +
        draw_label(
            round(cor(df$CONSUMO, df$STUDIO), digits = 3),
            fontface = 'bold',
            x = 0.5,
            y = 0.5,
            hjust = 0.5,
            vjust = 0.5,
            size = 20,
            color = "red"
        )
), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
cor2 <- plot + annotation_custom(ggplotGrob(
    ggdraw() +
        draw_label(
            round(cor(df$CONSUMO, df$HOMEVAL), digits = 3),
            fontface = 'bold',
            x = 0.5,
            y = 0.5,
            hjust = 0.5,
            vjust = 0.5,
            size = 20,
            color = "red"
        )
), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
cor3 <- plot + annotation_custom(ggplotGrob(
    ggdraw() +
        draw_label(
            round(cor(df$STUDIO, df$HOMEVAL), digits = 3),
            fontface = 'bold',
            x = 0.5,
            y = 0.5,
            hjust = 0.5,
            vjust = 0.5,
            size = 20,
            color = "red"
        )
), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
cor4 <- plot + annotation_custom(ggplotGrob(
    ggdraw() +
        draw_label(
            round(cor(df$CONSUMO, df$VALUX), digits = 3),
            fontface = 'bold',
            x = 0.5,
            y = 0.5,
            hjust = 0.5,
            vjust = 0.5,
            size = 20,
            color = "red"
        )
), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
cor5 <- plot + annotation_custom(ggplotGrob(
    ggdraw() +
        draw_label(
            round(cor(df$STUDIO, df$VALUX), digits = 3),
            fontface = 'bold',
            x = 0.5,
            y = 0.5,
            hjust = 0.5,
            vjust = 0.5,
            size = 20,
            color = "red"
        )
), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
cor6 <- plot + annotation_custom(ggplotGrob(
    ggdraw() +
        draw_label(
            round(cor(df$VALUX, df$HOMEVAL), digits = 3),
            fontface = 'bold',
            x = 0.5,
            y = 0.5,
            hjust = 0.5,
            vjust = 0.5,
            size = 20,
            color = "red"
        )
), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
sc1 <- plot + annotation_custom(ggplotGrob(
    ggplot(data = df, aes(x = STUDIO, y = CONSUMO)) +
        geom_point(shape=1) +
        theme_void() +
        xlab("") +
        ylab("") +
        geom_smooth(se = F, method = 'loess', formula = 'y ~ x', lwd = 0.75, col = "red") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
sc2 <- plot + annotation_custom(ggplotGrob(
    ggplot(data = df, aes(x = HOMEVAL, y = CONSUMO)) +
        geom_point(shape=1) +
        theme_void() +
        xlab("") +
        ylab("") +
        geom_smooth(se = F, method = 'loess', formula = 'y ~ x', lwd = 0.75, col = "red") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
sc3 <- plot + annotation_custom(ggplotGrob(
    ggplot(data = df, aes(x = HOMEVAL, y = STUDIO)) +
        geom_point(shape=1) +
        theme_void() +
        xlab("") +
        ylab("") +
        geom_smooth(se = F, method = 'loess', formula = 'y ~ x', lwd = 0.75, col = "red") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
sc4 <- plot + annotation_custom(ggplotGrob(
    ggplot(data = df, aes(x = VALUX, y = CONSUMO)) +
        geom_point(shape=1) +
        theme_void() +
        xlab("") +
        ylab("") +
        geom_smooth(se = F, method = 'loess', formula = 'y ~ x', lwd = 0.75, col = "red") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
sc5 <- plot + annotation_custom(ggplotGrob(
    ggplot(data = df, aes(x = VALUX, y = STUDIO)) +
        geom_point(shape=1) +
        theme_void() +
        xlab("") +
        ylab("") +
        geom_smooth(se = F, method = 'loess', formula = 'y ~ x', lwd = 0.75, col = "red") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
sc6 <- plot + annotation_custom(ggplotGrob(
    ggplot(data = df, aes(x = VALUX, y = HOMEVAL)) +
        geom_point(shape=1) +
        theme_void() +
        xlab("") +
        ylab("") +
        geom_smooth(se = F, method = 'loess', formula = 'y ~ x', lwd = 0.75, col = "red") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
plot_grid(title1, sc1, sc2, sc4,
          cor1, title2, sc3, sc5,
          cor2, cor3, title3, sc6,
          cor4, cor5, cor6, title4,
          nrow = 4)
# Relazione molto plausibile tra le esplicative e la risposta


# 2: Stima OLS: check diagnostico negli errori; sono spazialmente correlati? 
# Cosa manca? SAR, SEM, SARMA

# Costruzione del primo modello
fit <- lm(CONSUMO ~ STUDIO + VALUX + HOMEVAL, data = df)
summary(fit)

ggplot(data = df, aes(x = fitted.values(fit), y = resid(fit))) +
    geom_point(shape=1) +
    theme_bw() +
    xlab("Valori fittati") +
    ylab("Residui") +
    geom_hline(yintercept = 0, col = "black", lty = 2) +
    geom_smooth(se = F, method = 'loess', formula = 'y ~ x', lwd = 0.75, col = "red")
ggplot(data = df, mapping = aes(resid(fit))) +
    geom_histogram(aes(y =after_stat(density)),bins = 20, col = "black", fill = "yellow", alpha = 1) + 
    geom_density(linewidth = 0.8, fill = "pink", alpha = 0.3) +
    theme_bw() +
    xlab("Residui") +
    ylab("Densit�")
ggplot(data.frame(resid = rstandard(fit)),aes(sample = resid)) + 
    stat_qq(shape=1) +
    stat_qq_line(color = "red", linewidth = 1) +
    theme_bw() +
    xlab("Quantili teorici normale") +
    ylab("Quantili empirici")
# Guardando i residui sembra che le variabili descrivano la risposta
# Tuttavia � evidente che ci sia un effetto nel consumo che chiaramente non � descritto dal modello

# 3: Specificazione del modello con test RLM


# 4: Stima dell'opportuno modello spaziale e discussione