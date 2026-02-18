library(data.table)
library(tidyverse)
library(moments)
library(tseries)
library(FinTS)
library(xtable)
library(gridExtra)
library(dplyr)
library(moments)

dt <- fread("merged_close_20170101_20260217.csv")

dt[, date := as.POSIXct(date)]
setorder(dt, date)
setnames(dt,
         old = c("BTC/USD","ETH/USD","BNB/USD","XRP/USD","SOL/USD"),
         new = c("BTC","ETH","BNB","XRP","SOL"))

cols <- c("BTC","ETH","BNB","XRP","SOL")

for (col in cols) {
  dt[, paste0("r_", col) := log(get(col)) - shift(log(get(col)))]
}

logretornsBTC <- dt[!is.na(r_BTC), .(date, r_BTC)]
logretornsETH <- dt[!is.na(r_ETH), .(date, r_ETH)]
logretornsBNB <- dt[!is.na(r_BNB), .(date, r_BNB)]
logretornsXRP <- dt[!is.na(r_XRP), .(date, r_XRP)]
logretornsSOL <- dt[!is.na(r_SOL), .(date, r_SOL)]

lossBTC <- logretornsBTC[r_BTC < 0, .(date, loss = -r_BTC)]
lossETH <- logretornsETH[r_ETH < 0, .(date, loss = -r_ETH)]
lossBNB <- logretornsBNB[r_BNB < 0, .(date, loss = -r_BNB)]
lossXRP <- logretornsXRP[r_XRP < 0, .(date, loss = -r_XRP)]
lossSOL <- logretornsSOL[r_SOL < 0, .(date, loss = -r_SOL)]

lossdf <- rbindlist(list(
  cbind(lossBTC, crypto = "BTC"),
  cbind(lossETH, crypto = "ETH"),
  cbind(lossBNB, crypto = "BNB"),
  cbind(lossXRP, crypto = "XRP"),
  cbind(lossSOL, crypto = "SOL")
))

vlossBTC<-lossBTC$loss
vlossETH<-lossETH$loss
vlossBNB<-lossBNB$loss
vlossXRP<-lossXRP$loss
vlossSOL<-lossSOL$loss

tauladescriptiva <- lossdf %>%
  group_by(crypto) %>%
  summarise(
    N = n(),
    Mitjana = mean(loss),
    SD = sd(loss),
    Min = min(loss),
    Max = max(loss),
    Asimetria = skewness(loss),
    Curtosi = kurtosis(loss),
    JBstat = jarque.bera.test(loss)$statistic,
    JBpvalor = jarque.bera.test(loss)$p.value
  )

tauladescform <- tauladescriptiva %>%
  mutate(across(where(is.numeric), ~ formatC(.x, format = "e", digits = 2)))

print(
  xtable(tauladescform,
         caption = "Taula descriptiva dels log-retorns de les pèrdues",
         label = "tab:desc"),
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity
)
