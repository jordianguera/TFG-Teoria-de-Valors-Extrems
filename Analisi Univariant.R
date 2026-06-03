library(evir)
library(ercv)
library(ismev)
library(POT)
library(knitr)

dfperdues1m <- lapply(llista1m, function(df) df$perd)
dfperdues1h <- lapply(llista1h, function(df) df$perd)
dfperdues1d <- lapply(llista1d, function(df) df$perd)

perduesBTC1m<-dfperdues1m$BTC[dfperdues1m$BTC>0]
perduesETH1m<-dfperdues1m$ETH[dfperdues1m$ETH>0]
perduesBNB1m<-dfperdues1m$BNB[dfperdues1m$BNB>0]
perduesXRP1m<-dfperdues1m$XRP[dfperdues1m$XRP>0]
perduesSOL1m<-dfperdues1m$SOL[dfperdues1m$SOL>0]

#com que te cues pesades el cvplot no es pot interpretar ja que no te moments finits
#idem com que la EVI esta per sobre dels punts suspensius, no hi ha moments finits

#les dades son laplace
#es pot observar que amb 100.000 mostres no excluides ja hi ha un bon threshold (aprox 50%)


criptos <- c("BTC","ETH","BNB","XRP","SOL")

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
}


set.seed(1714)

tdataBTC<-tdata(dfperdues1m$BTC[dfperdues1m$BTC>0])
tdataETH<-tdata(dfperdues1m$ETH[dfperdues1m$ETH>0])
tdataBNB<-tdata(dfperdues1m$BNB[dfperdues1m$BNB>0])
tdataSOL<-tdata(dfperdues1m$SOL[dfperdues1m$SOL>0])
tdataXRP<-tdata(dfperdues1m$XRP[dfperdues1m$XRP>0])

tdades <- list(
  BTC = tdataBTC,
  ETH = tdataETH,
  BNB = tdataBNB,
  SOL = tdataSOL,
  XRP = tdataXRP
)

#Cvplot de les dades transformades per trobar el llindar on comença la cua
for (nom in names(tdades)) {
  x <- na.omit(tdades[[nom]])
  png(filename = paste0("cvplot1_", nom, ".png"),
      width = 1200,
      height = 800,
      res = 150)
  
  cvplot(x)
  dev.off()
}

#Zoom dels cvplots dibuixant l'interval de confiança del cv per trobar el llindar aproximat

png(filename = paste0("cvplot_BNB.png"),
  width = 1200,
  height = 800,
  res = 150  )
ne <- ceiling(length(tdades[["BNB"]]) * 0.001)

cvplot(tdades[["BNB"]], nextremes = ne, evi = evicv(0.82))
dev.off()

#Un llindar adequat per BNB per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1947750 observacions excloses

png(filename = paste0("cvplot_BTC.png"),
    width = 1200,
    height = 800,
    res = 150  )
ne <- ceiling(length(tdades[["BTC"]]) * 0.004)

cvplot(tdades[["BTC"]], nextremes = ne, evi = evicv(0.8))
dev.off()

#Un llindar adequat per BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 2190000 observacions excloses


png(filename = paste0("cvplot_ETH.png"),
    width = 1200,
    height = 800,
    res = 150  )
ne <- ceiling(length(tdades[["ETH"]]) * 0.004)

cvplot(tdades[["ETH"]], nextremes = ne, evi = evicv(0.82))
dev.off()

#Un llindar adequat per ETH per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 2170000 observacions excloses


png(filename = paste0("cvplot_SOL.png"),
    width = 1200,
    height = 800,
    res = 150  )
ne <- ceiling(length(tdades[["SOL"]]) * 0.003)

cvplot(tdades[["SOL"]], nextremes = ne, evi = evicv(0.88))
dev.off()

#Un llindar adequat per SOL per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1350000 observacions excloses


png(filename = paste0("cvplot_XRP.png"),
    width = 1200,
    height = 800,
    res = 150  )
ne <- ceiling(length(tdades[["XRP"]]) * 0.0005)

dev.off()

#Un llindar adequat per XRP per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1896000 observacions excloses

set.seed(1714)
nextremesBNB<-(length(tdataBNB)-1948000)
nextremesBTC<-(length(tdataBTC)-2190000)
nextremesETH<-(length(tdataETH)-2170000)
nextremesSOL<-(length(tdataSOL)-1350000)
nextremesXRP<-(length(tdataXRP)-1896000)

eviBNBaprox<-fitpot(tdataBNB, nextremes = nextremesBNB)$coef[["evi"]]
eviBTCaprox<-fitpot(tdataBTC, nextremes = nextremesBTC)$coef[["evi"]]
eviETHaprox<-fitpot(tdataETH, nextremes = nextremesETH)$coef[["evi"]]
eviSOLaprox<-fitpot(tdataSOL, nextremes = nextremesSOL)$coef[["evi"]]
eviXRPaprox<-fitpot(tdataXRP, nextremes = nextremesXRP)$coef[["evi"]]

thrBNBaprox<-thrselect(tdataBNB, evi = eviBNBaprox)
thrBTCaprox<-thrselect(tdataBTC, evi = eviBTCaprox)
thrETHaprox<-thrselect(tdataETH, evi = eviETHaprox)
thrSOLaprox<-thrselect(tdataSOL, evi = eviSOLaprox)
thrXRPaprox<-thrselect(tdataXRP, evi = eviXRPaprox)

fitBNB<-fitpot(dfperdues1m$BNB, nextremes = thrBNBaprox$solution$nextremes)
fitBTC<-fitpot(dfperdues1m$BTC, nextremes = thrBTCaprox$solution$nextremes)
fitETH<-fitpot(dfperdues1m$ETH, nextremes = thrETHaprox$solution$nextremes)
fitSOL<-fitpot(dfperdues1m$SOL, nextremes = thrSOLaprox$solution$nextremes)
fitXRP<-fitpot(dfperdues1m$XRP, nextremes = thrXRPaprox$solution$nextremes)

criptos <- c("BTC", "ETH", "BNB", "XRP", "SOL")

for (cripto in criptos) {
  fit <- get(paste0("fit", cripto))
  
  png(filename = paste0("ccdf_", cripto, ".png"),
    width = 1200,
    height = 800,
    res = 150)
  
  ccdfplot(dfperdues1m[[cripto]], pars = fit,
           main = paste("ccdf ", cripto))
  
  dev.off()
  
  png(filename = paste0("ccdf_logxy_", cripto, ".png"),
    width = 1200,
    height = 800,
    res = 150)
  
  ccdfplot(dfperdues1m[[cripto]],pars = fit, log = "xy",
          main = paste("ccdf ", cripto, "(loglog)"))
  dev.off()
}

VaR<-list()
for (cripto in criptos){
  fit <- get(paste0("fit", cripto))
  nivells <- c(0.9999, 0.999925, 0.99995, 0.999975, 0.99999)
  vars <- sapply(nivells, function(q) {
    v <- qpot(1 - q, pars = fit$coeff, lower.tail = FALSE)
    cat("VaR(", q * 100, "%) =", round(v, 6), "\n")
    v
  })
  names(vars) <- paste0("VaR ", nivells * 100, "%")
  VaR[[cripto]]<- vars
}

VaR

taulaVaR <- do.call(rbind, VaR)
taulaVaR <- round(taulaVaR, 6)

kable(
  taulaVaR,
  format = "latex",
  booktabs = TRUE,
  caption = "VaR per cada nivell",
  label = "varpot",
  align = "lccccc"
)

