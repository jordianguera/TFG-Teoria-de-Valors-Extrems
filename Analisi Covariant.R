library(evir)
library(ercv)
library(ismev)
library(POT)
library(MASS)
library(fitdistrplus)
library(data.table)
library(xtable)


set.seed(1714)


basetemps <- Reduce(function(x, y) merge(x, y, by = "data", all = FALSE),
                    list(BTC[, .(data)],
                      ETH[, .(data)],
                      BNB[, .(data)],
                      XRP[, .(data)],
                      SOL[, .(data)]))

alinear <- function(df, base) {
  df <- merge(base, df, by = "data", all.x = TRUE)
  setorder(df, data)
  df[, ret := log(tanca) - shift(log(tanca))]
  df[, perd := -ret]
  df <- df[is.finite(ret) & !is.na(ret)]
  return(df)
}

llista1m <- list(
  BTC = alinear(BTC, basetemps),
  ETH = alinear(ETH, basetemps),
  BNB = alinear(BNB, basetemps),
  XRP = alinear(XRP, basetemps),
  SOL = alinear(SOL, basetemps)
)

dfperdues1m <- lapply(llista1m, function(df) df$perd)




llindarBTC<-fitBTC$coeff[["threshold"]]
propextremsBTC<-sum(dfperdues1m$BTC > llindarBTC)/length(dfperdues1m$BTC)


# Condicionat a BTC >99.9%

BTCquant9990<-quantile(dfperdues1m$BTC, 0.9990)

perduesETHcond9990<- dfperdues1m$ETH[dfperdues1m$BTC > BTCquant9990 & dfperdues1m$ETH>0]
perduesBNBcond9990<- dfperdues1m$BNB[dfperdues1m$BTC > BTCquant9990 & dfperdues1m$BNB>0]
perduesSOLcond9990<- dfperdues1m$SOL[dfperdues1m$BTC > BTCquant9990 & dfperdues1m$SOL>0]
perduesXRPcond9990<- dfperdues1m$XRP[dfperdues1m$BTC > BTCquant9990 & dfperdues1m$XRP>0]

cvplot(perduesETHcond9990, evi = evicv(1.4))
#Un llindar adequat per ETH condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 2000 observacions excloses

cvplot(perduesBNBcond9990, evi = evicv(1.4))
#Un llindar adequat per BNB condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 2000 observacions excloses

cvplot(perduesSOLcond9990, evi = evicv(1.4))
#Un llindar adequat per SOL condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 2000 observacions excloses

cvplot(perduesXRPcond9990, nextremes = 500, evi = evicv(1.1))
#Un llindar adequat per XRP condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 2800 observacions excloses


set.seed(1714)
nextremesETHcond9990<-(length(perduesETHcond9990)-2000)
nextremesBNBcond9990<-(length(perduesBNBcond9990)-2000)
nextremesSOLcond9990<-(length(perduesSOLcond9990)-2000)
nextremesXRPcond9990<-(length(perduesXRPcond9990)-2750)

eviETHcondaprox9990<-fitpot(perduesETHcond9990, nextremes = nextremesETHcond9990)$coef[["evi"]]
eviBNBcondaprox9990<-fitpot(perduesBNBcond9990, nextremes = nextremesBNBcond9990)$coef[["evi"]]
eviSOLcondaprox9990<-fitpot(perduesSOLcond9990, nextremes = nextremesSOLcond9990)$coef[["evi"]]
eviXRPcondaprox9990<-fitpot(perduesXRPcond9990, nextremes = nextremesXRPcond9990)$coef[["evi"]]

thrETHcondaprox9990<-thrselect(perduesETHcond9990, evi = eviETHcondaprox9990)
thrBNBcondaprox9990<-thrselect(perduesBNBcond9990, evi = eviBNBcondaprox9990)
thrSOLcondaprox9990<-thrselect(perduesSOLcond9990, evi = eviSOLcondaprox9990)
thrXRPcondaprox9990<-thrselect(perduesXRPcond9990, evi = eviXRPcondaprox9990)

fitETHcond9990<-fitpot(perduesETHcond9990, nextremes = thrETHcondaprox9990$solution$nextremes)
fitBNBcond9990<-fitpot(perduesBNBcond9990, nextremes = thrBNBcondaprox9990$solution$nextremes)
fitSOLcond9990<-fitpot(perduesSOLcond9990, nextremes = thrSOLcondaprox9990$solution$nextremes)
fitXRPcond9990<-fitpot(perduesXRPcond9990, nextremes = thrXRPcondaprox9990$solution$nextremes)






# Condicionat a BTC >99.91%
BTCquant9991<-quantile(dfperdues1m$BTC, 0.9991)

perduesETHcond9991<- dfperdues1m$ETH[dfperdues1m$BTC > BTCquant9991 & dfperdues1m$ETH>0]
perduesBNBcond9991<- dfperdues1m$BNB[dfperdues1m$BTC > BTCquant9991 & dfperdues1m$BNB>0]
perduesSOLcond9991<- dfperdues1m$SOL[dfperdues1m$BTC > BTCquant9991 & dfperdues1m$SOL>0]
perduesXRPcond9991<- dfperdues1m$XRP[dfperdues1m$BTC > BTCquant9991 & dfperdues1m$XRP>0]

cvplot(perduesETHcond9991, evi = evicv(1.3))
#Un llindar adequat per ETH condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 2000 observacions excloses

cvplot(perduesBNBcond9991, evi = evicv(1.4))
#Un llindar adequat per BNB condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 2000 observacions excloses

cvplot(perduesSOLcond9991, evi = evicv(1.4))
#Un llindar adequat per SOL condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 2000 observacions excloses

cvplot(perduesXRPcond9991, nextremes = 500, evi = evicv(1.1))
#Un llindar adequat per XRP condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 2500 observacions excloses


set.seed(1714)
nextremesETHcond9991<-(length(perduesETHcond9991)-2000)
nextremesBNBcond9991<-(length(perduesBNBcond9991)-2000)
nextremesSOLcond9991<-(length(perduesSOLcond9991)-2000)
nextremesXRPcond9991<-(length(perduesXRPcond9991)-2500)

eviETHcondaprox9991<-fitpot(perduesETHcond9991, nextremes = nextremesETHcond9991)$coef[["evi"]]
eviBNBcondaprox9991<-fitpot(perduesBNBcond9991, nextremes = nextremesBNBcond9991)$coef[["evi"]]
eviSOLcondaprox9991<-fitpot(perduesSOLcond9991, nextremes = nextremesSOLcond9991)$coef[["evi"]]
eviXRPcondaprox9991<-fitpot(perduesXRPcond9991, nextremes = nextremesXRPcond9991)$coef[["evi"]]

thrETHcondaprox9991<-thrselect(perduesETHcond9991, evi = eviETHcondaprox9991)
thrBNBcondaprox9991<-thrselect(perduesBNBcond9991, evi = eviBNBcondaprox9991)
thrSOLcondaprox9991<-thrselect(perduesSOLcond9991, evi = eviSOLcondaprox9991)
thrXRPcondaprox9991<-thrselect(perduesXRPcond9991, evi = eviXRPcondaprox9991)

fitETHcond9991<-fitpot(perduesETHcond9991, nextremes = thrETHcondaprox9991$solution$nextremes)
fitBNBcond9991<-fitpot(perduesBNBcond9991, nextremes = thrBNBcondaprox9991$solution$nextremes)
fitSOLcond9991<-fitpot(perduesSOLcond9991, nextremes = thrSOLcondaprox9991$solution$nextremes)
fitXRPcond9991<-fitpot(perduesXRPcond9991, nextremes = thrXRPcondaprox9991$solution$nextremes)





# Condicionat a BTC >99.92%
BTCquant9992<-quantile(dfperdues1m$BTC, 0.9992)

perduesETHcond9992<- dfperdues1m$ETH[dfperdues1m$BTC > BTCquant9992 & dfperdues1m$ETH>0]
perduesBNBcond9992<- dfperdues1m$BNB[dfperdues1m$BTC > BTCquant9992 & dfperdues1m$BNB>0]
perduesSOLcond9992<- dfperdues1m$SOL[dfperdues1m$BTC > BTCquant9992 & dfperdues1m$SOL>0]
perduesXRPcond9992<- dfperdues1m$XRP[dfperdues1m$BTC > BTCquant9992 & dfperdues1m$XRP>0]

cvplot(perduesETHcond9992, evi = evicv(1.3))
#Un llindar adequat per ETH condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1500 observacions excloses

cvplot(perduesBNBcond9992, evi = evicv(1.4))
#Un llindar adequat per BNB condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1500 observacions excloses

cvplot(perduesSOLcond9992, evi = evicv(1.4))
#Un llindar adequat per SOL condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1800 observacions excloses

cvplot(perduesXRPcond9992, nextremes = 300, evi = evicv(1.1))
#Un llindar adequat per XRP condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 2175 observacions excloses



set.seed(1714)
nextremesETHcond9992<-(length(perduesETHcond9992)-1500)
nextremesBNBcond9992<-(length(perduesBNBcond9992)-1500)
nextremesSOLcond9992<-(length(perduesSOLcond9992)-1800)
nextremesXRPcond9992<-(length(perduesXRPcond9992)-2175)

eviETHcondaprox9992<-fitpot(perduesETHcond9992, nextremes = nextremesETHcond9992)$coef[["evi"]]
eviBNBcondaprox9992<-fitpot(perduesBNBcond9992, nextremes = nextremesBNBcond9992)$coef[["evi"]]
eviSOLcondaprox9992<-fitpot(perduesSOLcond9992, nextremes = nextremesSOLcond9992)$coef[["evi"]]
eviXRPcondaprox9992<-fitpot(perduesXRPcond9992, nextremes = nextremesXRPcond9992)$coef[["evi"]]

thrETHcondaprox9992<-thrselect(perduesETHcond9992, evi = eviETHcondaprox9992)
thrBNBcondaprox9992<-thrselect(perduesBNBcond9992, evi = eviBNBcondaprox9992)
thrSOLcondaprox9992<-thrselect(perduesSOLcond9992, evi = eviSOLcondaprox9992)
thrXRPcondaprox9992<-thrselect(perduesXRPcond9992, evi = eviXRPcondaprox9992)

fitETHcond9992<-fitpot(perduesETHcond9992, nextremes = thrETHcondaprox9992$solution$nextremes)
fitBNBcond9992<-fitpot(perduesBNBcond9992, nextremes = thrBNBcondaprox9992$solution$nextremes)
fitSOLcond9992<-fitpot(perduesSOLcond9992, nextremes = thrSOLcondaprox9992$solution$nextremes)
fitXRPcond9992<-fitpot(perduesXRPcond9992, nextremes = thrXRPcondaprox9992$solution$nextremes)




# Condicionat a BTC >99.93%
BTCquant9993<-quantile(dfperdues1m$BTC, 0.9993)

perduesETHcond9993<- dfperdues1m$ETH[dfperdues1m$BTC > BTCquant9993 & dfperdues1m$ETH>0]
perduesBNBcond9993<- dfperdues1m$BNB[dfperdues1m$BTC > BTCquant9993 & dfperdues1m$BNB>0]
perduesSOLcond9993<- dfperdues1m$SOL[dfperdues1m$BTC > BTCquant9993 & dfperdues1m$SOL>0]
perduesXRPcond9993<- dfperdues1m$XRP[dfperdues1m$BTC > BTCquant9993 & dfperdues1m$XRP>0]

cvplot(perduesETHcond9993, evi = evicv(1.4))
#Un llindar adequat per ETH condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1500 observacions excloses

cvplot(perduesBNBcond9993, evi = evicv(1.4))
#Un llindar adequat per BNB condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1500 observacions excloses

cvplot(perduesSOLcond9993, evi = evicv(1.4))
#Un llindar adequat per SOL condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1500 observacions excloses

cvplot(perduesXRPcond9993, nextremes = 500, evi = evicv(1.1))
#Un llindar adequat per XRP condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1900 observacions excloses



set.seed(1714)
nextremesETHcond9993<-(length(perduesETHcond9993)-1500)
nextremesBNBcond9993<-(length(perduesBNBcond9993)-1500)
nextremesSOLcond9993<-(length(perduesSOLcond9993)-1500)
nextremesXRPcond9993<-(length(perduesXRPcond9993)-1900)

eviETHcondaprox9993<-fitpot(perduesETHcond9993, nextremes = nextremesETHcond9993)$coef[["evi"]]
eviBNBcondaprox9993<-fitpot(perduesBNBcond9993, nextremes = nextremesBNBcond9993)$coef[["evi"]]
eviSOLcondaprox9993<-fitpot(perduesSOLcond9993, nextremes = nextremesSOLcond9993)$coef[["evi"]]
eviXRPcondaprox9993<-fitpot(perduesXRPcond9993, nextremes = nextremesXRPcond9993)$coef[["evi"]]

thrETHcondaprox9993<-thrselect(perduesETHcond9993, evi = eviETHcondaprox9993)
thrBNBcondaprox9993<-thrselect(perduesBNBcond9993, evi = eviBNBcondaprox9993)
thrSOLcondaprox9993<-thrselect(perduesSOLcond9993, evi = eviSOLcondaprox9993)
thrXRPcondaprox9993<-thrselect(perduesXRPcond9993, evi = eviXRPcondaprox9993)

fitETHcond9993<-fitpot(perduesETHcond9993, nextremes = thrETHcondaprox9993$solution$nextremes)
fitBNBcond9993<-fitpot(perduesBNBcond9993, nextremes = thrBNBcondaprox9993$solution$nextremes)
fitSOLcond9993<-fitpot(perduesSOLcond9993, nextremes = thrSOLcondaprox9993$solution$nextremes)
fitXRPcond9993<-fitpot(perduesXRPcond9993, nextremes = thrXRPcondaprox9993$solution$nextremes)




# Condicionat a BTC >99.94%
BTCquant9994<-quantile(dfperdues1m$BTC, 0.9994)

perduesETHcond9994<- dfperdues1m$ETH[dfperdues1m$BTC > BTCquant9994 & dfperdues1m$ETH>0]
perduesBNBcond9994<- dfperdues1m$BNB[dfperdues1m$BTC > BTCquant9994 & dfperdues1m$BNB>0]
perduesSOLcond9994<- dfperdues1m$SOL[dfperdues1m$BTC > BTCquant9994 & dfperdues1m$SOL>0]
perduesXRPcond9994<- dfperdues1m$XRP[dfperdues1m$BTC > BTCquant9994 & dfperdues1m$XRP>0]

cvplot(perduesETHcond9994, evi = evicv(1.4))
#Un llindar adequat per ETH condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1400 observacions excloses

cvplot(perduesBNBcond9994, evi = evicv(1.3))
#Un llindar adequat per BNB condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1500 observacions excloses

cvplot(perduesSOLcond9994, evi = evicv(1.2))
#Un llindar adequat per SOL condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1200 observacions excloses

cvplot(perduesXRPcond9994, nextremes = 200, evi = evicv(1.1))
#Un llindar adequat per XRP condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1650 observacions excloses



set.seed(1714)
nextremesETHcond9994<-(length(perduesETHcond9994)-1400)
nextremesBNBcond9994<-(length(perduesBNBcond9994)-1500)
nextremesSOLcond9994<-(length(perduesSOLcond9994)-1200)
nextremesXRPcond9994<-(length(perduesXRPcond9994)-1650)

eviETHcondaprox9994<-fitpot(perduesETHcond9994, nextremes = nextremesETHcond9994)$coef[["evi"]]
eviBNBcondaprox9994<-fitpot(perduesBNBcond9994, nextremes = nextremesBNBcond9994)$coef[["evi"]]
eviSOLcondaprox9994<-fitpot(perduesSOLcond9994, nextremes = nextremesSOLcond9994)$coef[["evi"]]
eviXRPcondaprox9994<-fitpot(perduesXRPcond9994, nextremes = nextremesXRPcond9994)$coef[["evi"]]

thrETHcondaprox9994<-thrselect(perduesETHcond9994, evi = eviETHcondaprox9994)
thrBNBcondaprox9994<-thrselect(perduesBNBcond9994, evi = eviBNBcondaprox9994)
thrSOLcondaprox9994<-thrselect(perduesSOLcond9994, evi = eviSOLcondaprox9994)
thrXRPcondaprox9994<-thrselect(perduesXRPcond9994, evi = eviXRPcondaprox9994)

fitETHcond9994<-fitpot(perduesETHcond9994, nextremes = thrETHcondaprox9994$solution$nextremes)
fitBNBcond9994<-fitpot(perduesBNBcond9994, nextremes = thrBNBcondaprox9994$solution$nextremes)
fitSOLcond9994<-fitpot(perduesSOLcond9994, nextremes = thrSOLcondaprox9994$solution$nextremes)
fitXRPcond9994<-fitpot(perduesXRPcond9994, nextremes = thrXRPcondaprox9994$solution$nextremes)






# Condicionat a BTC >99.95%
BTCquant9995<-quantile(dfperdues1m$BTC, 0.9995)

perduesETHcond9995<- dfperdues1m$ETH[dfperdues1m$BTC > BTCquant9995 & dfperdues1m$ETH>0]
perduesBNBcond9995<- dfperdues1m$BNB[dfperdues1m$BTC > BTCquant9995 & dfperdues1m$BNB>0]
perduesSOLcond9995<- dfperdues1m$SOL[dfperdues1m$BTC > BTCquant9995 & dfperdues1m$SOL>0]
perduesXRPcond9995<- dfperdues1m$XRP[dfperdues1m$BTC > BTCquant9995 & dfperdues1m$XRP>0]

cvplot(perduesETHcond9995, evi = evicv(1.3))
#Un llindar adequat per ETH condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1000 observacions excloses

cvplot(perduesBNBcond9995, evi = evicv(1.4))
#Un llindar adequat per BNB condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1200 observacions excloses

cvplot(perduesSOLcond9995, evi = evicv(1.2))
#Un llindar adequat per SOL condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1200 observacions excloses

cvplot(perduesXRPcond9995, nextremes = 200, evi = evicv(1.1))
#Un llindar adequat per XRP condicionat a valors extrems de BTC per ajustar un fitpot per trobar l'evi aproximat sembla ser a prop de les 1350 observacions excloses



set.seed(1714)
nextremesETHcond9995<-(length(perduesETHcond9995)-1000)
nextremesBNBcond9995<-(length(perduesBNBcond9995)-1200)
nextremesSOLcond9995<-(length(perduesSOLcond9995)-1000)
nextremesXRPcond9995<-(length(perduesXRPcond9995)-1350)

eviETHcondaprox9995<-fitpot(perduesETHcond9995, nextremes = nextremesETHcond9995)$coef[["evi"]]
eviBNBcondaprox9995<-fitpot(perduesBNBcond9995, nextremes = nextremesBNBcond9995)$coef[["evi"]]
eviSOLcondaprox9995<-fitpot(perduesSOLcond9995, nextremes = nextremesSOLcond9995)$coef[["evi"]]
eviXRPcondaprox9995<-fitpot(perduesXRPcond9995, nextremes = nextremesXRPcond9995)$coef[["evi"]]

thrETHcondaprox9995<-thrselect(perduesETHcond9995, evi = eviETHcondaprox9995)
thrBNBcondaprox9995<-thrselect(perduesBNBcond9995, evi = eviBNBcondaprox9995)
thrSOLcondaprox9995<-thrselect(perduesSOLcond9995, evi = eviSOLcondaprox9995)
thrXRPcondaprox9995<-thrselect(perduesXRPcond9995, evi = eviXRPcondaprox9995)

fitETHcond9995<-fitpot(perduesETHcond9995, nextremes = thrETHcondaprox9995$solution$nextremes)
fitBNBcond9995<-fitpot(perduesBNBcond9995, nextremes = thrBNBcondaprox9995$solution$nextremes)
fitSOLcond9995<-fitpot(perduesSOLcond9995, nextremes = thrSOLcondaprox9995$solution$nextremes)
fitXRPcond9995<-fitpot(perduesXRPcond9995, nextremes = thrXRPcondaprox9995$solution$nextremes)




#CoVaR

criptos <- c("ETH", "BNB", "SOL", "XRP")
quantil <- c("9990", "9991", "9992", "9993", "9994", "9995")
CoVaR <- list()
for (cond in quantil) {
  CoVaR[[cond]] <- list()
  for (cripto in criptos) {
    fit <- get(paste0("fit", cripto, "cond", cond))
    nivells <- c(0.9999, 0.999925, 0.99995, 0.999975, 0.99999)
    CoVaRs <- sapply(nivells, function(q) {
      qpot(1 - q, pars = fit$coeff, lower.tail = FALSE)
    })
    names(CoVaRs) <- paste0("CoVaR ", nivells * 100, "%")
    CoVaR[[cond]][[cripto]] <- CoVaRs
  }
}


CoVaR



quantils <- c("9990", "9991", "9992", "9993", "9994", "9995")
criptos <- c("ETH", "BNB", "SOL", "XRP")

nivells <- 1:length(CoVaR[[quantils[1]]][[criptos[1]]])

nomnivell <- c(
  "CoVaR Cripto > 99.9% | BTC > X",
  "CoVaR Cripto > 99.925% | BTC > X",
  "CoVaR Crypto > 99.95% | BTC > X",
  "CoVaR Cripto > 99.975% | BTC > X",
  "CoVaR Cripto > 99.999% | BTC > X"
)


for (i in nivells) {
  
  taula <- do.call(rbind, lapply(quantils, function(q) {
    
    data.frame(
      qBTC = as.numeric(q)/10000,
      
      ETH = CoVaR[[q]]$ETH[i],
      BNB = CoVaR[[q]]$BNB[i],
      SOL = CoVaR[[q]]$SOL[i],
      XRP = CoVaR[[q]]$XRP[i],
      
      dETH = CoVaR[[q]]$ETH[i] - VaR$ETH[i],
      dBNB = CoVaR[[q]]$BNB[i] - VaR$BNB[i],
      dSOL = CoVaR[[q]]$SOL[i] - VaR$SOL[i],
      dXRP = CoVaR[[q]]$XRP[i] - VaR$XRP[i]
    )
    
  }))
  
  cat("\n% ", nomnivell[i], "\n")
  
  print(
    xtable(
      taula,
      digits = 4,
      caption = paste0("CoVaR i $\\Delta$CoVaR per al nivell ", nomnivell[i]),
      label = paste0("tab:covar_", i)
    ),
    include.rownames = FALSE,
    floating = TRUE,
    caption.placement = "top"
  )
}

