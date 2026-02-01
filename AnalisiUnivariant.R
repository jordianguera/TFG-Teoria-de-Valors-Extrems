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

lossBTC <- -logretornsBTC[logretornsBTC<0]
lossETH <- -logretornsETH[logretornsETH<0]
lossBNB <- -logretornsBNB[logretornsBNB<0]
lossXRP <- -logretornsXRP[logretornsXRP<0]
lossSOL <- -logretornsSOL[logretornsSOL<0]


lossdf <- data.table(
  loss = c(lossBTC, lossETH, lossBNB, lossXRP, lossSOL),
  crypto = rep(c("BTC","ETH","BNB","XRP","SOL"),
               times = c(length(lossBTC),
                         length(lossETH),
                         length(lossBNB),
                         length(lossXRP),
                         length(lossSOL)))
)


# Analisi Univariant

library(dplyr)

desc_table <- lossdf %>%
  group_by(crypto) %>%
  summarise(
    n = n(),
    mean = mean(loss),
    sd = sd(loss),
    min = min(loss),
    max = max(loss)
  )

print(desc_table)

library(ggplot2)

ggplot(lossdf, aes(x = loss)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 80,
                 fill = "grey70",
                 color = "black") +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~crypto, scales = "free") +
  labs(
    title = "Distribució de les pèrdues (log-retorns negatius)",
    x = "Loss",
    y = "Densitat"
  ) +
  theme_minimal()

ggplot(lossdf, aes(x = crypto, y = loss)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log1p") +
  labs(
    title = "Comparació de pèrdues extremes (escala log)",
    x = "",
    y = "Loss"
  ) +
  theme_minimal()

ggplot(lossdf, aes(x = loss, color = crypto)) +
  stat_ecdf(linewidth = 1) +
  scale_x_continuous(limits = quantile(lossdf$loss, c(0, 0.995))) +
  labs(
    title = "Funcions de distribució empíriques (zona central)",
    x = "Loss",
    y = "F(x)"
  ) +
  theme_minimal()

loss_tail <- lossdf %>%
  group_by(crypto) %>%
  filter(loss > quantile(loss, 0.99))

ggplot(loss_tail, aes(x = loss, color = crypto)) +
  stat_ecdf(linewidth = 1) +
  labs(
    title = "Comportament de les cues (percentil 99+)",
    x = "Loss",
    y = "F(x)"
  ) +
  theme_minimal()


# Correlacions

losses <- na.omit(cbind(lossBTC, lossETH, lossBNB, lossXRP, lossSOL))
colnames(losses) <- c("BTC","ETH","BNB","XRP","SOL")

cor(losses)

cor(losses, method = "spearman")



# Selecció del llindar (POT) BTC

library(evir)
library(ercv)
library(ismev)

cvplot(lossBTC)

u_seq <- quantile(lossBTC, seq(0.8, 0.999, by = 0.001))

xi_hat <- sapply(u_seq, function(u) {
  fit <- gpd.fit(lossBTC, threshold = u, show = FALSE)
  fit$mle[2]
})

plot(u_seq, xi_hat, type = "b",
     xlab = "Llindar",
     ylab = "ksi",
     main = "Estabilitat del paràmetre de cua (BTC)")
abline(h = mean(xi_hat), col = "red")


# Selecció del llindar (POT) ETH

library(evir)
library(ercv)
library(ismev)

cvplot(lossETH)

u_seq <- quantile(lossETH, seq(0.8, 0.999, by = 0.001))

xi_hat <- sapply(u_seq, function(u) {
  fit <- gpd.fit(lossETH, threshold = u, show = FALSE)
  fit$mle[2]
})

plot(u_seq, xi_hat, type = "b",
     xlab = "Llindar",
     ylab = "ksi",
     main = "Estabilitat del paràmetre de cua (ETH)")
abline(h = mean(xi_hat), col = "red")


# Selecció del llindar (POT) BNB

library(evir)
library(ercv)
library(ismev)

cvplot(lossBNB)

u_seq <- quantile(lossBNB, seq(0.8, 0.999, by = 0.001))

xi_hat <- sapply(u_seq, function(u) {
  fit <- gpd.fit(lossBNB, threshold = u, show = FALSE)
  fit$mle[2]
})

plot(u_seq, xi_hat, type = "b",
     xlab = "Llindar",
     ylab = "ksi",
     main = "Estabilitat del paràmetre de cua (BNB)")
abline(h = mean(xi_hat), col = "red")


# Selecció del llindar (POT) XRP

library(evir)
library(ercv)
library(ismev)

cvplot(lossXRP)

u_seq <- quantile(lossXRP, seq(0.8, 0.999, by = 0.001))

xi_hat <- sapply(u_seq, function(u) {
  fit <- gpd.fit(lossXRP, threshold = u, show = FALSE)
  fit$mle[2]
})

plot(u_seq, xi_hat, type = "b",
     xlab = "Llindar",
     ylab = "ksi",
     main = "Estabilitat del paràmetre de cua (XRP)")
abline(h = mean(xi_hat), col = "red")


# Selecció del llindar (POT) SOL

library(evir)
library(ercv)
library(ismev)

cvplot(lossSOL)

u_seq <- quantile(lossSOL, seq(0.8, 0.999, by = 0.001))

xi_hat <- sapply(u_seq, function(u) {
  fit <- gpd.fit(lossSOL, threshold = u, show = FALSE)
  fit$mle[2]
})

plot(u_seq, xi_hat, type = "b",
     xlab = "Llindar",
     ylab = "ksi",
     main = "Estabilitat del paràmetre de cua (SOL)")
abline(h = mean(xi_hat), col = "red")

# VaR
qpot(alfa, modBTC)

# ES
epot(alfa, modBTC)

cbind(
  Manual = VaRBTC,
  Funcio = qpot(alfa, modBTC)
)

# Ajust amb fpot

library(evir)

fitBTC_fpot <- fpot(lossBTC, threshold = uBTC)

fitBTC_fpot$estimate

rbind(
  gpd_fit = modBTC$mle,
  fpot = fitBTC_fpot$estimate
)


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

# Domini d'atracció del màxim

alpha <- 1 / params$xi
data.frame(params$criptomoneda, alpha)


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
