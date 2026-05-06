library(evir)
library(ercv)
library(ismev)
library(POT)

dfperdues1m <- lapply(llista1m, function(df) df$perd)
dfperdues1h <- lapply(llista1h, function(df) df$perd)
dfperdues1d <- lapply(llista1d, function(df) df$perd)

perduesBTC1m<-dfperdues1m$BTC[dfperdues1m$BTC>0]
perduesBTC1h<-dfperdues1h$BTC[dfperdues1h$BTC>0]
perduesBTC1d<-dfperdues1d$BTC[dfperdues1d$BTC>0]

# 1m
meplot(perduesBTC1m)
hill(perduesBTC1m)
POT::mrlplot(perduesBTC1m)
tcplot(perduesBTC1m)

cvplot(perduesBTC1m)
#com que te cues pesades el cvplot no es pot interpretar ja que no te moments finits
#idem com que la EVI esta per sobre dels punts suspensius, no hi ha moments finits

#per tant, es passa a tdata
dadest0 <- tdata(perduesBTC1m)

par(mfrow = c(1, 3))
cvplot(dadest0)
meplot(dadest0)
hill(dadest0)
#les dades son laplace
#es pot observar que amb 100.000 mostres no excluides ja hi ha un bon threshold (aprox 50%)

par(mfrow = c(1, 1))

# Threshold amb thrselect (es ho fa amb simulacio o sigui que defineixo una seed)
set.seed(1714)
tselectBTC1m<-thrselect(perduesBTC1m)
tselectBTC1m

uBTC1m<-tselectBTC1m$solution$threshold
pBTC1m<-perduesBTC1m[perduesBTC1m>uBTC1m]
print(paste0(round(100*length(pBTC1m)/length(perduesBTC1m),3),"%"))

par(mfrow = c(1, 1))
fitBTC1m <- fitpot(perduesBTC1m, threshold = uBTC1m)
print(fitBTC1m)

# QVaR
nivells <- c(0.98, 0.99, 0.999)
varsBTC1m <- sapply(nivells, function(p) {
  v <- qpot(1 - p, pars = fitBTC1m$coeff, lower.tail = FALSE)
  cat("VaR(", p * 100, "%) =", round(v, 6), "\n")
  v
})
names(varsBTC1m) <- paste0("VaR ", nivells * 100, "%")
print(varsBTC1m)




criptos <- c("BNB")
resultats1m <- list()

for (cripto in criptos) {
  cat(cripto, "1m\n")
  
  perdues <- dfperdues1m[[cripto]]
  perdues <- perdues[perdues > 0]
  
  png(paste0(cripto, "_1m_cues.png"), width = 1200, height = 800)
  par(mfrow = c(2, 3))
  meplot(perdues, main = paste(cripto, "1m - meplot"))
  hill(perdues, main = paste(cripto, "1m - hill"))
  POT::mrlplot(perdues, main = paste(cripto, "1m - mrlplot"))
  tcplot(perdues)
  cvplot(perdues, main = paste(cripto, "1m - cvplot"))
  dev.off()
  
  # Transformacio tdata
  dadest0 <- tdata(perdues)
  png(paste0(cripto, "_1m_tdata.png"), width = 1200, height = 400)
  par(mfrow = c(1, 3))
  cvplot(dadest0, main = paste(cripto, "tdata - cvplot"))
  meplot(dadest0, main = paste(cripto, "tdata - meplot"))
  hill(dadest0, main = paste(cripto, "tdata - hill"))
  dev.off()
  
  # Seleccio de threshold
  set.seed(1714)
  tsel <- thrselect(perdues)
  u <- tsel$solution$threshold
  p <- perdues[perdues > u]
  cat("Threshold:", round(u, 6), "\n")
  cat("Excedencies:", paste0(round(100 * length(p) / length(perdues), 3), "%"), "\n")
  
  # Ajust GPD
  fit <- fitpot(perdues, threshold = u)
  print(fit)
  
  # VaR
  nivells <- c(0.98, 0.99, 0.999)
  vars <- sapply(nivells, function(q) {
    v <- qpot(1 - q, pars = fit$coeff, lower.tail = FALSE)
    cat("VaR(", q * 100, "%) =", round(v, 6), "\n")
    v
  })
  names(vars) <- paste0("VaR ", nivells * 100, "%")
  
  resultats1m[[cripto]] <- list(
    threshold = u,
    pct_excedencies = round(100 * length(p) / length(perdues), 3),
    fit = fit,
    VaR = vars
  )
}




# 1h
meplot(perduesBTC1h)
hill(perduesBTC1h)
POT::mrlplot(perduesBTC1h)
tcplot(perduesBTC1h)
cvplot(perduesBTC1h)

# 1d
meplot(perduesBTC1d)
hill(perduesBTC1d)
POT::mrlplot(perduesBTC1d)
tcplot(perduesBTC1d)
cvplot(perduesBTC1d)



