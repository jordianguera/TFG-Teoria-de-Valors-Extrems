library(data.table)
library(tidyverse)
library(moments)
library(tseries)
library(FinTS)
library(xtable)
library(gridExtra)
library(zoo)
library(scales)
library(evir)
library(ercv)
library(ismev)
library(POT)

# 6.2 Histogrames pos/neg

hlog <- function(ret, nom, freq, barres = 50, logstr = "") {
  logx <- grepl("x", logstr)
  logy <- grepl("y", logstr)
  
  mkbreaks <- function(x) {
    if (logx) exp(seq(log(min(x)), log(max(x)), length.out = barres + 1))
    else       barres
  }
  
  grafich <- function(h, colorbarra, main, xlab) {
    xl <- h$breaks[-length(h$breaks)]
    xr <- h$breaks[-1]
    d  <- h$density
    ok <- d > 0
    ybot <- if (logy) min(d[ok]) * 0.5 else 0
    ylim <- c(ybot, max(d[ok]) * if (logy) 2 else 1.05)
    plot(NA, xlim = range(h$breaks), ylim = ylim, log = logstr,
         main = main, xlab = xlab, ylab = "Densitat")
    rect(xl[ok], ybot, xr[ok], d[ok],
         col = adjustcolor(colorbarra, alpha.f = 0.7), border = NA)
  }
  
  pos <- ret[ret > 0]
  grafich(hist(pos, breaks = mkbreaks(pos), plot = FALSE),
            "steelblue", paste0(nom, " + (", freq, ")"), "ret > 0")
  
  neg <- abs(ret[ret < 0])
  grafich(hist(neg, breaks = mkbreaks(neg), plot = FALSE),
            "tomato", paste0(nom, " - (", freq, ")"), "|ret < 0|")
}

for (freq in names(freqs)) {
  for (cfg in list(c("","lin"), c("y","lnY"), c("x","lnX"), c("xy","lnXY"))) {
    png(paste0("HistRet_", cfg[2], "_", freq, ".png"), width = 2000, height = 3000, res = 200)
    par(mfrow = c(5,2), mar = c(4,4,3,1))
    for (n in noms) hlog(freqs[[freq]][[n]]$ret, n, freq, logstr = cfg[1])
    dev.off()
  }
}
# Funció de supervivència empírica - panels agrupats per freqüència i escala

ecdfsupervivencia <- function(x) {
  xs <- sort(x)
  n  <- length(xs)
  list(x = xs, y = 1 - seq_len(n) / n)
}

configs <- list(
  list(logstr = "",   tag = "lin"),
  list(logstr = "y",  tag = "lnY"),
  list(logstr = "x",  tag = "lnX"),
  list(logstr = "xy", tag = "lnXY")
)



basetemps <- Reduce(function(x, y) merge(x, y, by = "data", all = FALSE),
                    list(BTC[, .(data)],
                         ETH[, .(data)],
                         BNB[, .(data)],
                         XRP[, .(data)],
                         SOL[, .(data)]))
retornscont <- function(df, temps) {
  setorderv(df, temps)
  df[, ret := log(tanca) - shift(log(tanca))]
  df[, perd := -ret]
  df <- df[is.finite(ret) & !is.na(ret)]
  return(df)
}

alinear <- function(df, base) {
  df <- merge(base, df, by = "data", all.x = TRUE)
  setorder(df, data)
  df[, ret := log(tanca) - shift(log(tanca))]
  df[, perd := -ret]
  df <- df[is.finite(ret) & !is.na(ret)]
  return(df)
}

llista1m <- list(BTC = alinear(BTC, basetemps),
  ETH = alinear(ETH, basetemps),
  BNB = alinear(BNB, basetemps),
  XRP = alinear(XRP, basetemps),
  SOL = alinear(SOL, basetemps))

BTC[, hora := floor_date(data, "1 hour")]
ETH[, hora := floor_date(data, "1 hour")]
BNB[, hora := floor_date(data, "1 hour")]
XRP[, hora := floor_date(data, "1 hour")]
SOL[, hora := floor_date(data, "1 hour")]

basetemps1h <- Reduce(intersect, list(
  unique(BTC$hora),
  unique(ETH$hora),
  unique(BNB$hora),
  unique(XRP$hora),
  unique(SOL$hora)
))

BTC <- BTC[hora %in% basetemps1h]
ETH <- ETH[hora %in% basetemps1h]
BNB <- BNB[hora %in% basetemps1h]
XRP <- XRP[hora %in% basetemps1h]
SOL <- SOL[hora %in% basetemps1h]

btc1h <- retornscont(agrega_ohlc(BTC, "hora"), "hora")
eth1h <- retornscont(agrega_ohlc(ETH, "hora"), "hora")
bnb1h <- retornscont(agrega_ohlc(BNB, "hora"), "hora")
xrp1h <- retornscont(agrega_ohlc(XRP, "hora"), "hora")
sol1h <- retornscont(agrega_ohlc(SOL, "hora"), "hora")

BTC[, dia := as.Date(data)]
ETH[, dia := as.Date(data)]
BNB[, dia := as.Date(data)]
XRP[, dia := as.Date(data)]
SOL[, dia := as.Date(data)]

basetemps1d <- Reduce(intersect, list(
  unique(BTC$dia),
  unique(ETH$dia),
  unique(BNB$dia),
  unique(XRP$dia),
  unique(SOL$dia)
))

BTCd <- BTC[dia %in% basetemps1d]
ETHd <- ETH[dia %in% basetemps1d]
BNBd <- BNB[dia %in% basetemps1d]
XRPd <- XRP[dia %in% basetemps1d]
SOLd <- SOL[dia %in% basetemps1d]

btc1d <- retornscont(agrega_ohlc(BTCd, "dia"), "dia")
eth1d <- retornscont(agrega_ohlc(ETHd, "dia"), "dia")
bnb1d <- retornscont(agrega_ohlc(BNBd, "dia"), "dia")
xrp1d <- retornscont(agrega_ohlc(XRPd, "dia"), "dia")
sol1d <- retornscont(agrega_ohlc(SOLd, "dia"), "dia")


llista1m <- list(BTC = btc1m,
  ETH = eth1m,
  BNB = bnb1m,
  XRP = xrp1m,
  SOL = sol1m)

llista1h <- list(BTC = btc1h,
  ETH = eth1h,
  BNB = bnb1h,
  XRP = xrp1h,
  SOL = sol1h)

llista1d <- list(BTC = btc1d,
  ETH = eth1d,
  BNB = bnb1d,
  XRP = xrp1d,
  SOL = sol1d)


freqs <- list("1min" = llista1m, "1hora" = llista1h, "1dia" = llista1d)


for (freq in names(freqs)) {
  for (cfg in configs) {
    
    png(paste0("Survival_", cfg$tag, "_", freq, ".png"),
        width = 2000, height = 3000, res = 200)
    
    par(mfrow = c(5, 2), mar = c(4, 4, 3, 1))
    
    for (n in noms) {
      ret <- freqs[[freq]][[n]]$ret
      
      sp <- ecdfsupervivencia(ret[ret > 0])
      sn <- ecdfsupervivencia(abs(ret[ret < 0]))
      
      plot(sp$x, sp$y,
           type = "l", lwd = 1.5,
           col  = "steelblue",
           log  = cfg$logstr,
           main = paste0(n, " + (", freq, ")"),
           xlab = "r > 0",
           ylab = "S(x)",
           panel.first = grid(col = "grey85", lty = 1))
      
      plot(sn$x, sn$y,
           type = "l", lwd = 1.5,
           col  = "tomato",
           log  = cfg$logstr,
           main = paste0(n, " - (", freq, ")"),
           xlab = "|r < 0|",
           ylab = "S(x)",
           panel.first = grid(col = "grey85", lty = 1))
    }
    
    dev.off()
  }
}

# 6.2.4 Parells 2 a 2

plotparlog <- function(r1, r2, nom1, nom2, fitxer = "parells", nbins = 100, freq = "") {
  n <- min(length(r1), length(r2))
  r1 <- r1[1:n]; r2 <- r2[1:n]
  
  casos <- list(
    list(-r1[r1<0 & r2>0],  r2[r1<0 & r2>0],  paste0(nom1,"-"), paste0(nom2,"+"), TRUE,  FALSE), # TL
    list( r1[r1>0 & r2>0],  r2[r1>0 & r2>0],  paste0(nom1,"+"), paste0(nom2,"+"), FALSE, FALSE), # TR
    list(-r1[r1<0 & r2<0], -r2[r1<0 & r2<0],  paste0(nom1,"-"), paste0(nom2,"-"), TRUE,  TRUE),  # BL
    list( r1[r1>0 & r2<0], -r2[r1>0 & r2<0],  paste0(nom1,"+"), paste0(nom2,"-"), FALSE, TRUE)   # BR
  )
  
  mats <- vector("list", 4)
  bxs <- vector("list", 4)
  bys <- vector("list", 4)
  valid <- logical(4)
  
  for (i in seq_along(casos)) {
    cas <- casos[[i]]
    x <- cas[[1]]; y <- cas[[2]]
    x <- x[is.finite(x) & x > 0]
    y <- y[is.finite(y) & y > 0]
    if (length(x) < 2 || length(y) < 2) next
    valid[i] <- TRUE
    
    bx <- exp(seq(log(min(x)), log(max(x)), length.out = nbins + 1))
    by <- exp(seq(log(min(y)), log(max(y)), length.out = nbins + 1))
    m  <- matrix(0L, nbins, nbins)
    xi <- pmax(1, pmin(nbins, findInterval(x, bx, rightmost.closed = TRUE)))
    yi <- pmax(1, pmin(nbins, findInterval(y, by, rightmost.closed = TRUE)))
    for (j in seq_along(xi)) m[xi[j], yi[j]] <- m[xi[j], yi[j]] + 1L
    
    mats[[i]] <- m; bxs[[i]] <- bx; bys[[i]] <- by
  }
  
  zlim <- range(unlist(lapply(mats[valid], log1p)), na.rm = TRUE)
  cols <- hcl.colors(256, "YlOrRd", rev = TRUE)
  
  png(paste0(fitxer, "_log.png"), width = 1650, height = 1400, res = 150)
  layout(
    matrix(c(1, 2, 5,
             3, 4, 5), nrow = 2, ncol = 3, byrow = TRUE),
    widths = c(1, 1, 0.12)
  )
  
  for (i in seq_along(casos)) {
    par(mar = c(4, 4, 3, 1))
    cas <- casos[[i]]
    
    if (!valid[i]) { plot.new(); next }
    
    bx <- bxs[[i]]; by <- bys[[i]]; m <- mats[[i]]
    xinv <- cas[[5]];  yinv <- cas[[6]]
    
    lbx <- log(bx); lby <- log(by)
    xlimit <- if (xinv) rev(range(lbx)) else range(lbx)
    ylimit <- if (yinv) rev(range(lby)) else range(lby)
    
    atx <- pretty(lbx); aty <- pretty(lby)
    
    image(lbx, lby, log1p(m),
          col  = cols,
          zlim = zlim,
          xlim = xlimit,
          ylim = ylimit,
          main = paste0(cas[[3]], " vs ", cas[[4]], if (nchar(freq)) paste0("  [", freq, "]")),
          xlab = cas[[3]], ylab = cas[[4]],
          axes = FALSE)
    axis(1, at = atx, labels = formatC(exp(atx), format = "e", digits = 1))
    axis(2, at = aty, labels = formatC(exp(aty), format = "e", digits = 1))
    box()
  }
  
  par(mar = c(4, 0.5, 3, 3.5))
  cby <- seq(zlim[1], zlim[2], length.out = 256)
  image(x = 1, y = cby,
        z = matrix(cby, nrow = 1),
        col = cols, axes = FALSE, xlab = "", ylab = "")
  axis(4, las = 1, cex.axis = 0.75)
  mtext("log(1+n)", side = 4, line = 2.8, cex = 0.8)
  box()
  
  dev.off()
}


plotparlin <- function(r1, r2, nom1, nom2, nbins=300, freq="") {
  n <- min(length(r1), length(r2))
  r1 <- r1[1:n]; r2 <- r2[1:n]
  ok <- is.finite(r1) & is.finite(r2)
  x <- r1[ok]; y <- r2[ok]
  bx <- seq(min(x), max(x), length.out = (nbins+1))
  by <- seq(min(y), max(y), length.out = (nbins+1))
  m <- matrix(0L, nbins, nbins)
  xi <- pmax(1, pmin(nbins, findInterval(x, bx, rightmost.closed = TRUE)))
  yi <- pmax(1, pmin(nbins, findInterval(y, by, rightmost.closed = TRUE)))
  for (i in seq_along(xi)) m[xi[i], yi[i]] <- m[xi[i], yi[i]] + 1L
  image(bx, by, log1p(m), col = hcl.colors(nbins, "YlOrRd", rev = TRUE),
        main = paste0(nom1, " vs ", nom2, " ", freq),
        xlab = nom1,
        ylab = nom2)
}

parells <- combn(noms, 2, simplify = FALSE)

for (freq in names(freqs)) {
  for (p in parells) {
    d1 <- freqs[[freq]][[p[1]]]$ret
    d2 <- freqs[[freq]][[p[2]]]$ret
    plotparlog(d1, d2, p[1], p[2], fitxer = paste0("parells_", p[1], "_", p[2], "_", freq), freq=freq)
  }
  
  for (grp in list(list(1:5,"1"), list(6:10,"2"))) {
    png(paste0("parells", grp[[2]], "_lin_", freq, ".png"), width = 1800, height = 1200, res = 150)
    par(mfrow = c(2,3), mar = c(4,4,3,1))
    for (p in parells[grp[[1]]]) {
      d1 <- freqs[[freq]][[p[1]]]$ret
      d2 <- freqs[[freq]][[p[2]]]$ret
      plotparlin(d1, d2, p[1], p[2], freq=freq)
    }
    dev.off()
  }
}

# verificació
set.seed(1714)
x <- rnorm(10000)
y <- rnorm(10000)

plotparlog(x, y, "X", "Y", fitxer = "verif_gauss")
png("verif_gauss_lin.png", width = 900, height = 900, res = 150)
plotparlin(x, y, "X", "Y")
dev.off()