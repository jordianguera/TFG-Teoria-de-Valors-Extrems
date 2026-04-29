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


dades_t0 <- tdata(perduesBTC1m)
cvplot(dades_t0)
meplot(dades_t0)
hill(dades_t0)

dades_t3 <- tdata(perduesBTC1m, threshold = 0.03)
dades_t35 <- tdata(perduesBTC1m, threshold = 0.035)
dades_t4 <- tdata(perduesBTC1m, threshold = 0.04)

datasets <- list(dades_t3, dades_t35, dades_t4)

for (dades_t in datasets) {
  par(mfrow = c(1, 3))
  
  cvplot(dades_t, main = "CVplot BTC 1m - transformat", plot = TRUE)
  POT::mrlplot(dades_t, main = "MRLplot BTC 1m - transformat")
  hill(dades_t, main = "Hill BTC 1m - transformat")
}

par(mfrow = c(1, 1))

# FITPOT
fitBTC1m <- fitpot(perduesBTC1m, threshold = 0.03)
print(fitBTC1m)

fitBTC1m <- fitpot(perduesBTC1m, threshold = 0.04)
print(fitBTC1m)

fitBTC1m <- fitpot(perduesBTC1m, threshold = 0.035)
print(fitBTC1m)

# Threshold amb thrselect
tselect<-thrselect(perduesBTC1m)
u<-tselect$solution$threshold
dades_u <- tdata(perduesBTC1m, threshold = u)
par(mfrow = c(1, 3))

cvplot(dades_u, main = "CVplot BTC 1m - transformat", plot = TRUE)
POT::mrlplot(dades_u, main = "MRLplot BTC 1m - transformat")
hill(dades_u, main = "Hill BTC 1m - transformat")

par(mfrow = c(1, 1))
fitBTC1m <- fitpot(perduesBTC1m, threshold = u)
print(fitBTC1m)

# QVaR
nivells <- c(0.95, 0.99, 0.999)
varsBTC1m <- sapply(nivells, function(p) {
  v <- qpot(1 - p, pars = fitBTC1m$coeff, lower.tail = FALSE)
  cat("VaR(", p * 100, "%) =", round(v, 6), "\n")
  v
})
names(varsBTC1m) <- paste0("VaR ", nivells * 100, "%")
print(varsBTC1m)

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



