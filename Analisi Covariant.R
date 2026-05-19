library(evir)
library(ercv)
library(ismev)
library(POT)
library(MASS)
library(fitdistrplus)

alineat <- Reduce(
  function(a, b) merge(a, b, by = "data"),
  lapply(c("BTC", alts), function(cr) {
    setNames(llista1m[[cr]][, c("data", "perd")], c("data", paste0("perd", cr)))
  })
)
uBTC  <- 0.001308866
idx   <- which(alineat$perdBTC > uBTC)
nivells <- c(0.99, 0.991, 0.992, 0.993, 0.994, 0.995, 0.996, 0.997, 0.998, 0.999)

taula <- data.frame(nivell = nivells)

for (a in alts) {
  perdCond <- alineat[[paste0("perd", a)]][idx]
  perdCond <- perdCond[perdCond > 0]
  fit      <- fitpot(perdCond, threshold = quantile(perdCond, 0.90))
  taula[[a]] <- sapply(nivells, function(q)
    qpot(1 - q, pars = fit$coeff, lower.tail = FALSE))
}

print(round(taula, 6))

criptos <- c("BTC", "BNB", "ETH", "SOL", "XRP")
alts    <- c("ETH", "BNB", "XRP", "SOL")


ajustagpd <- function(perdues, nivells = NULL, u = NULL) {
  perdues <- perdues[perdues > 0]
  set.seed(1714)
  if (is.null(u)) u <- thrselect(perdues)$solution$threshold
  fit <- fitpot(perdues, threshold = u)
  res <- list(u = u, fit = fit, pctexc = 100 * mean(perdues > u))
  if (!is.null(nivells))
    res$VaR <- setNames(
      sapply(nivells, function(q) qpot(1 - q, pars = fit$coeff, lower.tail = FALSE)),
      paste0("VaR ", nivells * 100, "%")
    )
  res
}

# 1. GPD + VaR per cripto (1m)

resultats1m <- list()

uBTCfix <- 0.001308866

for (cripto in criptos) {
  cat(cripto, "1m\n")
  
  gpd <- ajustagpd(dfperdues1m[[cripto]], nivells = c(0.98, 0.99, 0.999),
                    u = if (cripto == "BTC") uBTCfix else NULL)
  
  cat("Threshold:", round(gpd$u, 6), "\n")
  cat("Excedències:", round(gpd$pctexc, 3), "%\n")
  print(gpd$fit)
  for (nm in names(gpd$VaR)) cat(nm, "=", round(gpd$VaR[[nm]], 6), "\n")
  
  resultats1m[[cripto]] <- gpd
}

# 2. CDF empírica vs GPD (BTC 1m)

fitBTC1m  <- resultats1m$BTC$fit
uBTC1m    <- resultats1m$BTC$u
xiBTC1m   <- fitBTC1m$coeff[["evi"]]
sigmaBTC1m <- fitBTC1m$coeff[["psi"]]

excBTC1m <- sort(perduesBTC1m[perduesBTC1m > uBTC1m] - uBTC1m)
nExc     <- length(excBTC1m)

png("BTC_1m_CDF_GPD.png", width = 1000, height = 700)
plot(excBTC1m, (1:nExc) / (nExc + 1),
     main = "BTC 1m - CDF empírica vs GPD", xlab = "Excedència", ylab = "CDF",
     pch = 16, cex = 0.5, col = "steelblue")
lines(excBTC1m, POT::pgpd(excBTC1m, loc = 0, scale = sigmaBTC1m, shape = xiBTC1m),
      col = "firebrick", lwd = 2)
legend("bottomright", bty = "n", legend = c("Empírica", "GPD ajustada"),
       col = c("steelblue", "firebrick"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
dev.off()

# 3. T-Student forçada vs MLE (BTC 1m)

nuBTC1m <- 1 / xiBTC1m
cat("\nEVI BTC:", round(xiBTC1m, 4), "  nu forçat:", round(nuBTC1m, 4), "\n")

nllt <- function(params, x, nu = NULL) {
  mu    <- params[1]
  sigma <- params[2]
  if (sigma <= 0) return(Inf)
  nuFit <- if (is.null(nu)) { if (length(params) < 3 || params[3] <= 2) return(Inf); params[3] } else nu
  -sum(dt((x - mu) / sigma, df = nuFit, log = TRUE) - log(sigma))
}

optLliure <- optim(c(median(perduesBTC1m), mad(perduesBTC1m), 5),
                   nllt, x = perduesBTC1m,
                   method = "L-BFGS-B", lower = c(-Inf, 1e-8, 2.01))
muMle <- optLliure$par[1]
sigmaMle <- optLliure$par[2]
nuMle <- optLliure$par[3]
cat("\nt lliure mu:", round(muMle, 6), " sigma:", round(sigmaMle, 6),
    " nu:", round(nuMle, 6), "\n")

optForcat <- optim(c(median(perduesBTC1m), mad(perduesBTC1m)),
                   nllt, x = perduesBTC1m, nu = nuBTC1m,
                   method = "L-BFGS-B", lower = c(-Inf, 1e-8))
muT    <- optForcat$par[1]
sigmaT <- optForcat$par[2]
cat("t forçada mu:", round(muT, 6), " sigma:", round(sigmaT, 6), "\n")

xlimPlot <- quantile(perduesBTC1m, c(0, 0.995))
xSeq <- seq(xlimPlot[1], xlimPlot[2], length.out = 600)

png("BTC_1m_tstudent.png", width = 1000, height = 700)
hist(perduesBTC1m, breaks = 150, freq = FALSE,
     main = "BTC 1m - t-Student forçada vs MLE", xlab = "Pèrdua",
     col = "grey90", border = "grey70", xlim = xlimPlot)
lines(xSeq, dt((xSeq - muT)   / sigmaT,   df = nuBTC1m) / sigmaT,
      col = "firebrick", lwd = 2)
lines(xSeq, dt((xSeq - muMle) / sigmaMle, df = nuMle)   / sigmaMle,
      col = "steelblue", lwd = 2, lty = 2)
legend("topright", bty = "n",
       legend = c(paste0("t forçada nu=", round(nuBTC1m, 2)),
                  paste0("t MLE nu=",     round(nuMle,   2))),
       col = c("firebrick", "steelblue"), lty = c(1, 2), lwd = 2)
dev.off()

# 4. CoVaR multi-asset condicionat a BTC extrem

alineat <- Reduce(
  function(a, b) merge(a, b, by = "data"),
  lapply(c("BTC", alts), function(cr) {
    setNames(llista1m[[cr]][, c("data", "perd")], c("data", paste0("perd_", cr)))
  })
)

perdBTC <- alineat$perdBTC

varMarg <- sapply(alts, function(a) {
  ajustagpd(alineat[[paste0("perd", a)]], nivells = 0.99)$VaR[["VaR 99%"]]
})
cat("\nVaR marginals 99%:\n"); print(round(varMarg, 6))

nivellsBTC <- c(0.99, 0.991, 0.992, 0.993, 0.994, 0.995)
covarResultats <- list()

for (qBTC in nivellsBTC) {
  
  cat("BTC quantil:", qBTC, "\n")
  
  uBTC <- quantile(perdBTC, qBTC, na.rm = TRUE)
  idx  <- which(perdBTC > uBTC)
  cat("Threshold BTC:", round(uBTC, 6), "  Obs. condicionals:", length(idx), "\n")
  
  fila <- data.frame(qBTC = qBTC, varBTC = uBTC, nCond = length(idx))
  
  for (a in alts) {
    perdCond <- alineat[[paste0("perd", a)]][idx]
    perdCond <- perdCond[perdCond > 0]
    
    if (length(perdCond) < 100) { fila[[paste0("CoVaR", a)]] <- NA; next }
    
    gpdC <- ajustagpd(perdCond, nivells = 0.99)
    cat("  ", a, " threshold:", round(gpdC$u, 6),
        " excedències:", round(gpdC$pctexc, 2), "%",
        " CoVaR 99%:", round(gpdC$VaR[["VaR 99%"]], 6),
        " delta CoVaR:", round(gpdC$VaR[["VaR 99%"]] - varMarg[[a]], 6), "\n")
    print(gpdC$fit)
    
    fila[[paste0("CoVaR", a)]] <- gpdC$VaR[["VaR 99%"]]
    fila[[paste0("deltaCoVaR", a)]] <- gpdC$VaR[["VaR 99%"]] - varMarg[[a]]
  }
  
  covarResultats[[as.character(qBTC)]] <- fila
}

taulaCovar <- do.call(rbind, covarResultats)
taulaCovar <- taulaCovar[order(taulaCovar$qBTC), ]
print(taulaCovar)
write.csv(taulaCovar, "CoVaR_multi_crypto.csv", row.names = FALSE)

png("CoVaR_ETH_BTC.png", width = 1000, height = 700)
plot(taulaCovar$qBTC, taulaCovar$CoVaRETH,
     type = "b", pch = 16, lwd = 2,
     xlab = "Quantil BTC", ylab = "CoVaR ETH 99%",
     main = "CoVaR ETH condicionat a BTC extrem")
abline(h = varMarg[["ETH"]], lty = 2, col = "red")
legend("topleft", bty = "n",
       legend = c("CoVaR ETH", "VaR ETH marginal"),
       lty = c(1, 2), pch = c(16, NA), col = c("black", "red"))
dev.off()