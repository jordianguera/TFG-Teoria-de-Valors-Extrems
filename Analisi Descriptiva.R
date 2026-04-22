library(data.table)
library(tidyverse)
library(moments)
library(tseries)
library(FinTS)
library(xtable)
library(gridExtra)
library(zoo)
library(scales)

# 1. Càrrega de dades

carrega <- function(fitxer, nom) {
  df <- fread(fitxer)
  setnames(df, c("data", "ober", "max", "min", "tanca"))
  df[,data := as.POSIXct(data)]
  df[,tanca := as.numeric(tanca)]
  df[,ober := as.numeric(ober)]
  df[,max := as.numeric(max)]
  df[,min := as.numeric(min)]
  setorder(df, data)
  df[,crypto := nom]
  return(df)
}

BTC <- carrega("BTCUSD_1m_20170101_20260401.csv", "BTC")
ETH <- carrega("ETHUSD_1m_20170101_20260401.csv", "ETH")
BNB <- carrega("BNBUSD_1m_20170101_20260401.csv", "BNB")
XRP <- carrega("XRPUSD_1m_20170101_20260401.csv", "XRP")
SOL <- carrega("SOLUSD_1m_20170101_20260401.csv", "SOL")

datafinal <- as.POSIXct("2026-04-01 00:00:00", tz = "UTC")

BTC <- BTC[data <= datafinal]
ETH <- ETH[data <= datafinal]
BNB <- BNB[data <= datafinal]
XRP <- XRP[data <= datafinal]
SOL <- SOL[data <= datafinal]

noms <- c("BTC", "ETH", "BNB", "XRP", "SOL")

# 1 Neteja de dades

verifica <- function(df, nom) {
  cols <- c("data", "ober", "max", "min", "tanca")
  res <- sapply(cols, function(c) {
    x <- df[[c]]
    c(
      NAs  = sum(is.na(x)),
      NaNs = if (is.numeric(x)) sum(is.nan(x)) else 0L,
      Infs = if (is.numeric(x)) sum(is.infinite(x)) else 0L
    )
  })
  cat("\nQualitat dades:", nom, "\n")
  print(res)
  invisible(res)
}

verifica(BTC, "BTC")
verifica(ETH, "ETH")
verifica(BNB, "BNB")
verifica(XRP, "XRP")
verifica(SOL, "SOL")

# No hi ha NAs, NaNs ni Infs

# 2. Agregació per freqüència

BTC[, hora := floor_date(data, "1 hour")][, dia := as.Date(data, tz = "UTC")]
ETH[, hora := floor_date(data, "1 hour")][, dia := as.Date(data, tz = "UTC")]
BNB[, hora := floor_date(data, "1 hour")][, dia := as.Date(data, tz = "UTC")]
XRP[, hora := floor_date(data, "1 hour")][, dia := as.Date(data, tz = "UTC")]
SOL[, hora := floor_date(data, "1 hour")][, dia := as.Date(data, tz = "UTC")]

agrega_ohlc <- function(df, per_col) {
  df[, .(
    ober  = first(ober),
    max   = max(max),
    min   = min(min),
    tanca = last(tanca),
    crypto = first(crypto)
  ), by = per_col]
}

retornscont <- function(df, temps) {
  setorderv(df, temps)
  df[, ret  := log(tanca) - shift(log(tanca))]
  df[, perd := -ret]
  df <- df[!is.na(ret) & is.finite(ret)]
  return(df)
}

# 1 min
btc1m <- retornscont(copy(BTC), "data")
eth1m <- retornscont(copy(ETH), "data")
bnb1m <- retornscont(copy(BNB), "data")
xrp1m <- retornscont(copy(XRP), "data")
sol1m <- retornscont(copy(SOL), "data")

# 1 hora
btc1h <- retornscont(agrega_ohlc(BTC, "hora"), "hora")
eth1h <- retornscont(agrega_ohlc(ETH, "hora"), "hora")
bnb1h <- retornscont(agrega_ohlc(BNB, "hora"), "hora")
xrp1h <- retornscont(agrega_ohlc(XRP, "hora"), "hora")
sol1h <- retornscont(agrega_ohlc(SOL, "hora"), "hora")

# 1 dia
btc1d <- retornscont(agrega_ohlc(BTC, "dia"), "dia")
eth1d <- retornscont(agrega_ohlc(ETH, "dia"), "dia")
bnb1d <- retornscont(agrega_ohlc(BNB, "dia"), "dia")
xrp1d <- retornscont(agrega_ohlc(XRP, "dia"), "dia")
sol1d <- retornscont(agrega_ohlc(SOL, "dia"), "dia")

llista1m <- list(BTC=btc1m, ETH=eth1m, BNB=bnb1m, XRP=xrp1m, SOL=sol1m)
llista1h <- list(BTC=btc1h, ETH=eth1h, BNB=bnb1h, XRP=xrp1h, SOL=sol1h)
llista1d <- list(BTC=btc1d, ETH=eth1d, BNB=bnb1d, XRP=xrp1d, SOL=sol1d)

freqs <- list("1min" = llista1m, "1hora" = llista1h, "1dia" = llista1d)
temps <- list("1min" = "data", "1hora" = "hora", "1dia" = "dia")

# 3. Sèries temporals de preus

plotpreu <- function(df, nom, freq, col){
  ggplot(df, aes(x = .data[[col]], y = tanca)) +
    geom_line(color = "cyan2", linewidth = 0.4) +
    labs(
      title = paste0("Sèrie temporal del preu: ", nom, " (", freq, ")"),
      x = "Data", y = "Preu (USD)"
    ) +
    theme_minimal(base_size = 11)
}

for(freq in names(freqs)){
  ct <- temps[[freq]]
  plots <- lapply(noms, function(n) plotpreu(freqs[[freq]][[n]], n, freq, ct))
  pcomb <- do.call(grid.arrange, c(plots, ncol = 2,
                                   top = paste0("Sèries temporals dels preus - Freqüència ", freq)))
  ggsave(paste0("Preus_", freq, ".png"), plot = pcomb, width = 14, height = 16, dpi = 200)
}

# 4. Anàlisi numèric bàsic

analisinum <- function(df) {
  x <- df$ret
  data.frame(
    N = length(x),
    Mitjana = mean(x),
    Mediana = median(x),
    SD = sd(x),
    Min = min(x),
    Max = max(x),
    Q01 = quantile(x, 0.01),
    Q05 = quantile(x, 0.05),
    Q95 = quantile(x, 0.95),
    Q99 = quantile(x, 0.99)
  )
}

for (freq in names(freqs)) {
  taula <- do.call(rbind, lapply(noms, function(n) {
    r <- analisinum(freqs[[freq]][[n]])
    cbind(Crypto = n, r)
  }))
  rownames(taula) <- NULL
  
  # Taula 1: estadístics bàsics
  taula1 <- taula %>%
    select(Crypto, N, Mitjana, Mediana, SD, Min, Max) %>%
    mutate(
      N       = format(N, big.mark = ","),
      Mitjana = formatC(Mitjana, format = "e", digits = 3),
      Mediana = formatC(Mediana, format = "e", digits = 3),
      SD      = formatC(SD,      format = "e", digits = 3),
      Min     = round(Min, 5),
      Max     = round(Max, 5)
    )
  
  # Taula 2: quantils
  taula2 <- taula %>%
    select(Crypto, Q01, Q05, Q95, Q99) %>%
    mutate(
      Q01 = round(Q01, 5),
      Q05 = round(Q05, 5),
      Q95 = round(Q95, 5),
      Q99 = round(Q99, 5)
    )
  
  cat("\n\n Estadístics bàsics logretorns -", freq, "\n")
  print(
    xtable(taula1,
           caption = paste("Estadístics bàsics dels logretorns -", freq),
           label   = paste0("tab:basic_", freq)),
    include.rownames = FALSE,
    booktabs = TRUE,
    sanitize.text.function = identity
  )
  
  cat("\n\n Quantils logretorns -", freq, "\n")
  print(
    xtable(taula2,
           caption = paste("Quantils dels logretorns -", freq),
           label   = paste0("tab:quant_", freq)),
    include.rownames = FALSE,
    booktabs = TRUE,
    sanitize.text.function = identity
  )
}

# 5. Diagnòstic de cua

diagcua <- function(df, nom) {
  x <- df$ret
  jb <- jarque.bera.test(x)
  data.frame(
    Crypto = nom,
    Asimetria = round(skewness(x), 4),
    Curtosi = round(kurtosis(x), 4),
    JBstat = formatC(jb$statistic, format = "e", digits = 3),
    JBpval = ifelse(jb$p.value < 0.001, "<0.001", round(jb$p.value, 4))
  )
}

for (freq in names(freqs)) {
  taula <- do.call(rbind, lapply(noms, function(n) diagcua(freqs[[freq]][[n]], n)))
  rownames(taula) <- NULL
  cat("\n\n Diagnòstic de cua -", freq, "\n")
  print(
    xtable(taula,
           caption = paste("Asimetria, Curtosi i Jarque-Bera -", freq),
           label   = paste0("tab:cua_", freq)),
    include.rownames = FALSE,
    booktabs = TRUE,
    sanitize.text.function = identity
  )
}

# 6. Histogrames

# 6.1 Histogrames en escala numèrica

histretorns <- function(df, nom, freq) {
  ggplot(df, aes(x = ret)) +
    geom_histogram(aes(y = after_stat(density)), bins = 100,
                   fill = "cyan2", alpha = 0.7, color = NA) +
    geom_density(color = "darkblue", linewidth = 0.8) +
    stat_function(fun = dnorm, args = list(mean = mean(df$ret), sd = sd(df$ret)),
                  color = "red", linetype = "dashed", linewidth = 0.8) +
    labs(title = paste0("logretorns: ", nom, " (", freq, ")"),
         x = "logretorn", y = "Densitat") +
    theme_minimal(base_size = 10)
}

histperdues <- function(df, nom, freq) {
  ggplot(df, aes(x = perd)) +
    geom_histogram(aes(y = after_stat(density)), bins = 100,
                   fill = "darkred", alpha = 0.7, color = NA) +
    geom_density(color = "black", linewidth = 0.8) +
    stat_function(fun = dnorm, args = list(mean = mean(df$perd), sd = sd(df$perd)),
                  color = "orange", linetype = "dashed", linewidth = 0.8) +
    labs(title = paste0("Log-pèrdues: ", nom, " (", freq, ")"),
         x = "Log-pèrdua", y = "Densitat") +
    theme_minimal(base_size = 10)
}

for (freq in names(freqs)) {
  # Histogrames retorns
  plotsr <- lapply(noms, function(n) histretorns(freqs[[freq]][[n]], n, freq))
  pcomb  <- do.call(grid.arrange, c(plotsr, ncol = 2,
                                    top = paste0("Histogrames logretorns - Freqüència ", freq)))
  ggsave(paste0("HistRet_", freq, ".png"), plot = pcomb, width = 14, height = 16, dpi = 200)
  
  # Histogrames pèrdues
  plotsp <- lapply(noms, function(n) histperdues(freqs[[freq]][[n]], n, freq))
  pcomb2 <- do.call(grid.arrange, c(plotsp, ncol = 2,
                                    top = paste0("Histogrames log-pèrdues - Freqüència ", freq)))
  ggsave(paste0("HistPerd_", freq, ".png"), plot = pcomb2, width = 14, height = 16, dpi = 200)
}

# 6.2 Histogrames en escala log

#pseudo ln pels negatius
pln <- function(sigma) pseudo_log_trans(base = exp(1), sigma = sigma)


# 6.2.1. Eix Y en escala ln

histretornslny <- function(df, nom, freq) {
  ggplot(df, aes(x = ret)) +
    geom_histogram(aes(y = after_stat(density)), bins = 100,
                   fill = "blue2", alpha = 0.7, color = NA) +
    geom_density(color = "darkblue", linewidth = 0.8) +
    stat_function(fun = dnorm, args = list(mean = mean(df$ret), sd = sd(df$ret)),
                  color = "red", linetype = "dashed", linewidth = 0.8) +
    scale_y_continuous(trans = "log", labels = label_scientific()) +
    annotation_logticks(sides = "l") +
    labs(title = paste0(nom, " (", freq, ")"),
         x = "logretorn", y = "Densitat (ln)") +
    theme_minimal(base_size = 10)
}


# 6.2.2. Eix X en pseudo ln

histretornslnx <- function(df, nom, freq) {
  s <- sd(df$ret) * 0.1
  # el 0.1 fa que la transició cap al comportament log sigui ràpida, és només un ajust heurístic.
  
  ggplot(df, aes(x = ret)) +
    geom_histogram(aes(y = after_stat(density)), bins = 100,
                   fill = "cyan2", alpha = 0.7, color = NA) +
    geom_density(color = "darkblue", linewidth = 0.8) +
    stat_function(fun = dnorm, args = list(mean = mean(df$ret), sd = sd(df$ret)),
                  color = "red", linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(trans = pln(s),
                       labels = label_number(accuracy = 0.001)) +
    annotation_logticks(sides = "b") +
    labs(title = paste0(nom, " (", freq, ")"),
         x = "logretorn (pseudo-ln)", y = "Densitat") +
    theme_minimal(base_size = 10)
}

# 6.2.3. Ambdós eixos en ln i pseudo ln

histretornslnxy <- function(df, nom, freq) {
  s <- sd(df$ret) * 0.1
  ggplot(df, aes(x = ret)) +
    geom_histogram(aes(y = after_stat(density)), bins = 100,
                   fill = "cyan2", alpha = 0.7, color = NA) +
    geom_density(color = "darkblue", linewidth = 0.8) +
    stat_function(fun = dnorm, args = list(mean = mean(df$ret), sd = sd(df$ret)),
                  color = "red", linetype = "dashed", linewidth = 0.8) +
    scale_y_continuous(trans = "log", labels = label_scientific()) +
    scale_x_continuous(trans = pln(s),
                       labels = label_number(accuracy = 0.001)) +
    annotation_logticks(sides = "bl") +
    labs(title = paste0(nom, " (", freq, ")"),
         x = "logretorn (pseudo-ln)", y = "Densitat (ln)") +
    theme_minimal(base_size = 10)
}

# 6.2.4. Logretorns en parells 2 a 2 i eixos en escala ln

parellsdensitat <- function(llista, freq, coltemps) {
  parells <- combn(noms, 2, simplify = FALSE)
  meitat1 <- parells[1:5]
  meitat2 <- parells[6:10]
  
  plots2a2 <- function(par) {
    nom1 <- par[1]; nom2 <- par[2]
    df1 <- llista[[nom1]][, .(t = get(coltemps), ret1 = ret)]
    df2 <- llista[[nom2]][, .(t = get(coltemps), ret2 = ret)]
    dfm <- merge(df1, df2, by = "t")
    s1 <- sd(dfm$ret1) * 0.1
    s2 <- sd(dfm$ret2) * 0.1
    ggplot(dfm, aes(x = ret1, y = ret2)) +
      geom_bin2d(bins = 100) +
      scale_fill_viridis_c(
        option = "inferno", name = "n",
        trans  = "log",
        labels = label_scientific()
      ) +
      scale_x_continuous(trans = pln(s1),
                         labels = label_number(accuracy = 0.001)) +
      scale_y_continuous(trans = pln(s2),
                         labels = label_number(accuracy = 0.001)) +
      labs(title = paste0(nom1, " vs ", nom2), x = nom1, y = nom2) +
      theme_minimal(base_size = 9) +
      theme(legend.key.size = unit(0.4, "cm"),
            legend.title    = element_text(size = 7))
  }
  list(p1 = do.call(grid.arrange,
                    c(lapply(meitat1, plots2a2), ncol = 3,
                      top = paste0("Densitat retorns 2 a 2 (1/2) - ", freq))),
       p2 = do.call(grid.arrange,
                    c(lapply(meitat2, plots2a2), ncol = 3,
                      top = paste0("Densitat retorns 2 a 2 (2/2) - ", freq)))
  )
}

for (freq in names(freqs)) {
  ct <- temps[[freq]]
  # Y ln
  plotlny <- lapply(noms, function(n) histretornslny(freqs[[freq]][[n]], n, freq))
  ggsave(paste0("HistRetlnY_", freq, ".png"),
         plot = do.call(grid.arrange, c(plotlny, ncol = 2,
                                        top = paste0("Histogrames - Y ln - ", freq))),
         width = 14, height = 16, dpi = 200)
  
  # X pseudo ln
  plotlnx <- lapply(noms, function(n) histretornslnx(freqs[[freq]][[n]], n, freq))
  ggsave(paste0("HistRetlnX_", freq, ".png"),
         plot = do.call(grid.arrange, c(plotlnx, ncol = 2,
                                        top = paste0("Histogrames - X pseudo-ln - ", freq))),
         width = 14, height = 16, dpi = 200)
  
  # XY ln
  plotlnxy <- lapply(noms, function(n) histretornslnxy(freqs[[freq]][[n]], n, freq))
  ggsave(paste0("HistRetlnXY_", freq, ".png"),
         plot = do.call(grid.arrange, c(plotlnxy, ncol = 2,
                                        top = paste0("Histogrames - XY ln - ", freq))),
         width = 14, height = 16, dpi = 200)

  # parells densitat - 2 fitxers de 5 gràfics cadascun
  pars <- parellsdensitat(freqs[[freq]], freq, ct)
  ggsave(paste0("parellsdensitat1_", freq, ".png"),
         plot = pars$p1, width = 18, height = 12, dpi = 200)
  ggsave(paste0("parellsdensitat2_", freq, ".png"),
         plot = pars$p2, width = 18, height = 12, dpi = 200)

}


# 7. QQ-plot vs Normal

qqplot <- function(df, nom, freq, var, etiq) {
  ggplot(df, aes(sample = .data[[var]])) +
    stat_qq(color = "cyan2", alpha = 0.4, size = 0.5) +
    stat_qq_line(color = "red", linewidth = 0.9) +
    labs(title = paste0("QQ-plot ", etiq, ": ", nom, " (", freq, ")"),
         x = "Quantils teòrics (Normal)", y = "Quantils empírics") +
    theme_minimal(base_size = 10)
}

for (freq in names(freqs)) {
  # QQ retorns
  plotsr <- lapply(noms, function(n) qqplot(freqs[[freq]][[n]], n, freq, "ret", "logretorns"))
  pcomb <- do.call(grid.arrange, c(plotsr, ncol = 2,
                                     top = paste0("QQ-plots logretorns - Freqüència ", freq)))
  ggsave(paste0("QQRet_", freq, ".png"), plot = pcomb, width = 14, height = 16, dpi = 200)
  
  # QQ pèrdues
  plotsp <- lapply(noms, function(n) qqplot(freqs[[freq]][[n]], n, freq, "perd", "log-pèrdues"))
  pcomb2 <- do.call(grid.arrange, c(plotsp, ncol = 2,
                                     top = paste0("QQ-plots log-pèrdues - Freqüència ", freq)))
  ggsave(paste0("QQPerd_", freq, ".png"), plot = pcomb2, width = 14, height = 16, dpi = 200)
}

# 8. ACF dels logretorns

plotsacf <- function(df, nom, freq, var, etiq, max_lag = 40) {
  x <- df[[var]]
  acf_res <- acf(x, lag.max = max_lag, plot = FALSE)
  ic <- qnorm(0.975) / sqrt(length(x))
  taulaacf <- data.frame(
    retard = as.numeric(acf_res$lag[-1]),
    acf    = as.numeric(acf_res$acf[-1])
  )
  ggplot(taulaacf, aes(x = retard, y = acf)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_segment(aes(xend = retard, yend = 0), color = "cyan2") +
    geom_hline(yintercept = c(-ic, ic), linetype = "dashed", color = "red") +
    labs(title = paste0("ACF ", etiq, ": ", nom, " (", freq, ")"),
         x = "Retard", y = "ACF") +
    theme_minimal(base_size = 10)
}

for (freq in names(freqs)) {
  lagmax <- if (freq == "1min") 60 else if (freq == "1hora") 48 else 30
  
  plotsr <- lapply(noms, function(n)
    plotsacf(freqs[[freq]][[n]], n, freq, "ret", "logretorns", lagmax))
  pcomb <- do.call(grid.arrange, c(plotsr, ncol = 2,
                                     top = paste0("ACF logretorns - Freqüència ", freq)))
  ggsave(paste0("ACFRet_", freq, ".png"), plot = pcomb, width = 14, height = 16, dpi = 200)
}

# 9. Clustering de volatilitat

plotsvolcluster <- function(df, nom, freq, col) {
  df2 <- copy(df)
  df2[, absret := abs(ret)]
  ct  <- temps[[freq]]
  ggplot(df2, aes(x = .data[[ct]], y = absret)) +
    geom_line(color = "darkorange", linewidth = 0.3, alpha = 0.8) +
    labs(title = paste0("|logretorns|: ", nom, " (", freq, ")"),
         x = "Data", y = "|logretorn|") +
    theme_minimal(base_size = 10)
}

for (freq in names(freqs)) {
  ct <- temps[[freq]]
  plotsvol <- lapply(noms, function(n)
    plotsvolcluster(freqs[[freq]][[n]], n, freq, ct))
  pcomb  <- do.call(grid.arrange, c(plotsvol, ncol = 2,
                                     top = paste0("Volatility clustering (|ret|) - Freqüència ", freq)))
  ggsave(paste0("VolCluster_", freq, ".png"), plot = pcomb, width = 14, height = 16, dpi = 200)
}

# 10. ACF dels retorns absoluts

for (freq in names(freqs)) {
  lagmax <- if (freq == "1min") 60 else if (freq == "1hora") 48 else 30
  
  # ACF |retorns|
  plotsretornabs <- lapply(noms, function(n) {
    df2 <- copy(freqs[[freq]][[n]])
    df2[, absret := abs(ret)]
    plotsacf(df2, n, freq, "absret", "|logretorns|", lagmax)
  })
  pcomb <- do.call(grid.arrange, c(plotsretornabs, ncol = 2,
                                    top = paste0("ACF |logretorns| - Freqüència ", freq)))
  ggsave(paste0("ACFAbsRet_", freq, ".png"), plot = pcomb, width = 14, height = 16, dpi = 200)
  
  # ACF pèrdues
  plotsperd <- lapply(noms, function(n)
    plotsacf(freqs[[freq]][[n]], n, freq, "perd", "log-pèrdues", lagmax))
  pcomb2 <- do.call(grid.arrange, c(plotsperd, ncol = 2,
                                     top = paste0("ACF log-pèrdues - Freqüència ", freq)))
  ggsave(paste0("ACFPerd_", freq, ".png"), plot = pcomb2, width = 14, height = 16, dpi = 200)
}

# 11. Matrius de correlació

matriucorrelacio <- function(llista, freq, var) {
  # Alinea per data comuna
  ct <- temps[[freq]]
  dfs <- lapply(noms, function(n) {
    df <- llista[[n]][, c(ct, var), with = FALSE]
    setnames(df, c("tref", n))
    df
  })
  merged <- Reduce(function(a, b) merge(a, b, by = "tref", all = FALSE), dfs)
  mat    <- cor(merged[, -1, with = FALSE], use = "pairwise.complete.obs")
  return(mat)
}

plotscorrelacio <- function(mat, titol) {
  df_melt <- as.data.frame(as.table(mat))
  names(df_melt) <- c("X", "Y", "Corr")
  ggplot(df_melt, aes(x = X, y = Y, fill = Corr)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Corr, 2)), size = 4, fontface = "bold") +
    scale_fill_gradient2(low = "cyan2", mid = "white", high = "darkred",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(title = titol, x = "", y = "", fill = "Correlació") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

for (freq in names(freqs)) {
  # Correlació retorns
  matriuret  <- matriucorrelacio(freqs[[freq]], freq, "ret")
  plotsmatriuret    <- plotscorrelacio(matriuret,  paste0("Correlació logretorns (", freq, ")"))
  
  # Correlació pèrdues
  matriuperd <- matriucorrelacio(freqs[[freq]], freq, "perd")
  plotsmatriuperd   <- plotscorrelacio(matriuperd, paste0("Correlació log-pèrdues (", freq, ")"))
  
  pcomb <- grid.arrange(plotsmatriuret, plotsmatriuperd, ncol = 2)
  ggsave(paste0("Correlacio_", freq, ".png"), plot = pcomb, width = 14, height = 7, dpi = 200)
}