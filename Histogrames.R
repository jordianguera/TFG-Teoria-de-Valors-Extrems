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

hlog <- function(ret, nom, freq, logstr = "") {
  hp <- hist(ret[ret > 0],      breaks = 600, plot = FALSE)
  hn <- hist(abs(ret[ret < 0]), breaks = 600, plot = FALSE)
  dp <- hp$density > 0
  dn <- hn$density > 0
  plot(hp$mids[dp], hp$density[dp], type = "h", log = logstr,
       col = "steelblue", lwd = 1.5,
       main = paste0(nom, " + (", freq, ")"), xlab = "ret > 0", ylab = "Densitat")
  plot(hn$mids[dn], hn$density[dn], type = "h", log = logstr,
       col = "tomato", lwd = 1.5,
       main = paste0(nom, " - (", freq, ")"), xlab = "|ret < 0|", ylab = "Densitat")
}

for (freq in names(freqs)) {
  for (cfg in list(c("","lin"), c("y","lnY"), c("x","lnX"), c("xy","lnXY"))) {
    png(paste0("HistRet_", cfg[2], "_", freq, ".png"), width = 2000, height = 3000, res = 200)
    par(mfrow = c(5,2), mar = c(4,4,3,1))
    for (n in noms) hlog(freqs[[freq]][[n]]$ret, n, freq, logstr = cfg[1])
    dev.off()
  }
}


# 6.2.4 Parells 2 a 2

plotparlog <- function(r1, r2, nom1, nom2, fitxer = "parells", nbins=300, freq="") {
  n <- min(length(r1), length(r2))
  r1 <- r1[1:n]; r2 <- r2[1:n]
  
  casos <- list(
    list(r1[r1>0 & r2>0],  r2[r1>0 & r2>0],  paste0(nom1,"+"), paste0(nom2,"+")),
    list(-r1[r1<0 & r2>0], r2[r1<0 & r2>0],  paste0(nom1,"-"), paste0(nom2,"+")),
    list(r1[r1>0 & r2<0],  -r2[r1>0 & r2<0], paste0(nom1,"+"), paste0(nom2,"-")),
    list(-r1[r1<0 & r2<0], -r2[r1<0 & r2<0], paste0(nom1,"-"), paste0(nom2,"-"))
  )
  
  png(paste0(fitxer, "_log.png"), width = 1400, height = 1400, res = 150)
  par(mfrow = c(2,2), mar = c(4,4,3,1))
  for (cas in casos) {
    x <- cas[[1]]; y <- cas[[2]]
    x <- x[is.finite(x) & x > 0]; y <- y[is.finite(y) & y > 0]
    if (length(x) < 2 || length(y) < 2) { plot.new(); next }
    
    bx <- exp(seq(log(min(x)), log(max(x)), length.out = (nbins+1)))
    by <- exp(seq(log(min(y)), log(max(y)), length.out = (nbins+1)))
    m  <- matrix(0L, nbins, nbins)
    xi <- pmax(1, pmin(nbins, findInterval(x, bx, rightmost.closed = TRUE)))
    yi <- pmax(1, pmin(nbins, findInterval(y, by, rightmost.closed = TRUE)))
    for (i in seq_along(xi)) m[xi[i], yi[i]] <- m[xi[i], yi[i]] + 1L
    
    # eixos en log
    atx <- pretty(log(bx)); aty <- pretty(log(by))
    image(log(bx), log(by), log1p(m),
          col  = hcl.colors(nbins, "YlOrRd", rev = TRUE),
          main = paste0(cas[[3]], " vs ", cas[[4]], " ", freq),
          xlab = cas[[3]], ylab = cas[[4]],
          axes = FALSE)
    axis(1, at = atx, labels = round(exp(atx), 4))
    axis(2, at = aty, labels = round(exp(aty), 4))
    box()
  }
  dev.off()
}
plotparlin <- function(r1, r2, nom1, nom2, nbins=300, freq="") {
  n <- min(length(r1), length(r2))
  r1 <- r1[1:n]; r2 <- r2[1:n]
  ok <- is.finite(r1) & is.finite(r2)
  x <- r1[ok]; y <- r2[ok]
  bx <- seq(min(x), max(x), length.out = (nbins+1))
  by <- seq(min(y), max(y), length.out = (nbins+1))
  m  <- matrix(0L, nbins, nbins)
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