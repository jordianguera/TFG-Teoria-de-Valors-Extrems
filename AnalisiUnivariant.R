library(data.table)

carrega <- function(file) {
  dt <- fread(file)
  dt[, open_time := as.POSIXct(open_time)]
  setorder(dt, open_time)
  
  dt[,close:= as.numeric(close)]
  dt[,r:= log(close) - shift(log(close))]
  dt <- dt[!is.na(r)]
  return(dt$r)
}

logretornsBTC <- carrega("BTCUSDT_1m_365d.csv")
logretornsETH <- carrega("ETHUSDT_1m_365d.csv")
logretornsBNB <- carrega("BNBUSDT_1m_365d.csv")
logretornsXRP <- carrega("XRPUSDT_1m_365d.csv")
logretornsSOL <- carrega("SOLUSDT_1m_365d.csv")

lossBTC <- -logretornsBTC
lossETH <- -logretornsETH
lossBNB <- -logretornsBNB
lossXRP <- -logretornsXRP
lossSOL <- -logretornsSOL

# Llindar

library(evir)

meplot(lossBTC)
meplot(lossETH)
meplot(lossBNB)
meplot(lossXRP)
meplot(lossSOL)

uBTC <- quantile(lossBTC, 0.995)
uETH <- quantile(lossETH, 0.995)
uBNB <- quantile(lossBNB, 0.995)
uXRP <- quantile(lossXRP, 0.995)
uSOL <- quantile(lossSOL, 0.995)

# GPD

library(ismev)

modBTC <- gpd.fit(lossBTC, threshold = uBTC, show = FALSE)
modETH <- gpd.fit(lossETH, threshold = uETH, show = FALSE)
modBNB <- gpd.fit(lossBNB, threshold = uBNB, show = FALSE)
modXRP <- gpd.fit(lossXRP, threshold = uXRP, show = FALSE)
modSOL <- gpd.fit(lossSOL, threshold = uSOL, show = FALSE)

params <- data.frame(
  criptomoneda = c("BTC", "ETH", "BNB", "XRP", "SOL"),
  xi = c(modBTC$mle[2], modETH$mle[2], modBNB$mle[2], modXRP$mle[2], modSOL$mle[2]),
  beta = c(modBTC$mle[1], modETH$mle[1], modBNB$mle[1], modXRP$mle[1], modSOL$mle[1])
)

print(params)

# VaR i ES

alfa <- 0.999

VaR <- function(loss, u, fit, alfa) {
  N <- length(loss)
  Nu <- sum(loss > u)
  xi <- fit$mle[2]
  beta <- fit$mle[1]
  
  u + (beta / xi) * (((N / Nu) * (1 - alfa))^(-xi) - 1)
}

ES <- function(VaR, u, fit) {
  xi <- fit$mle[2]
  beta <- fit$mle[1]
  
  (VaR + (beta - xi * u)) / (1 - xi)
}

VaRBTC <- VaR(lossBTC, uBTC, modBTC, alfa)
VaRETH <- VaR(lossETH, uETH, modETH, alfa)
VaRBNB <- VaR(lossBNB, uBNB, modBNB, alfa)
VaRXRP <- VaR(lossXRP, uXRP, modXRP, alfa)
VaRSOL <- VaR(lossSOL, uSOL, modSOL, alfa)

ESBTC <- ES(VaRBTC, uBTC, modBTC)
ESETH <- ES(VaRETH, uETH, modETH)
ESBNB <- ES(VaRBNB, uBNB, modBNB)
ESXRP <- ES(VaRXRP, uXRP, modXRP)
ESSOL <- ES(VaRSOL, uSOL, modSOL)

taula <- data.frame(
  criptomoneda = c("BTC", "ETH", "BNB", "XRP", "SOL"),
  VaR99.9 = c(VaRBTC, VaRETH, VaRBNB, VaRXRP, VaRSOL),
  ES99.9  = c(ESBTC, ESETH, ESBNB, ESXRP, ESSOL),
  xi = params$xi
)

print(taula)

# Tots presenten xi >0 per tant tenen cues pesades (Frechet)

# Implica: moments alts potencialment infinits, presència d’esdeveniments extrems severs

# Ordre de risc extrem: 

# Segons xi: XRP > ETH=SOL=BNB > BTC​

# Segons VaR: XRP>SOL>ETH>BNB>BTC

# Segons ES: XRP>SOL>ETH>BNB>BTC
