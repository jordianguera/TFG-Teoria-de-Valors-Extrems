library(data.table)
library(tidyverse)
library(moments)
library(tseries)
library(FinTS)
library(xtable)
library(gridExtra)

# Funcio per carregar dades

carrega_crypto <- function(fitxer, nom){
  df <- fread(fitxer)
  setnames(df, c("date","open","max","min","close"))
  
  df[,date := as.POSIXct(date)]
  df[,close := as.numeric(close)]
  setorder(df, date)
  
  # logretorns
  df[, r := log(close) - shift(log(close))]
  df <- df[!is.na(r)]
  df[, crypto := nom]
  return(df)
}

# Carregar dades

BTC <- carrega_crypto("BTCUSD_1m_20170101_20260306.csv","BTC")
ETH <- carrega_crypto("ETHUSD_1m_20170101_20260306.csv","ETH")
BNB <- carrega_crypto("BNBUSD_1m_20170101_20260306.csv","BNB")
XRP <- carrega_crypto("XRPUSD_1m_20170101_20260306.csv","XRP")
SOL <- carrega_crypto("SOLUSD_1m_20170101_20260306.csv","SOL")

dfbind <- rbindlist(list(BTC,ETH,BNB,XRP,SOL))

dfbind[, loss := ifelse(r < 0, -r, NA)]
lossdf <- dfbind[!is.na(loss)]

tauladescriptiva <- lossdf %>%
  group_by(crypto) %>%
  summarise(
    N = n(),
    Mitjana = mean(loss),
    SD = sd(loss),
    Max = max(loss),
    Asimetria = skewness(loss),
    Curtosi = kurtosis(loss),
    JBstat = jarque.bera.test(loss)$statistic,
    JBpvalor = jarque.bera.test(loss)$p.value
  )

tauladescform <- tauladescriptiva %>%
  mutate(
    N = format(N, big.mark=","),
    
    # científica
    Mitjana = formatC(Mitjana, format="e", digits=2),
    SD = formatC(SD, format="e", digits=2),
    JBstat = formatC(JBstat, format="e", digits=2),
    
    # normal
    Max = round(Max,4),
    Asimetria = round(Asimetria,2),
    Curtosi = round(Curtosi,2),
    
    # només aquests arrodonits
    JBpvalor = ifelse(JBpvalor < 0.001,"<0.001",round(JBpvalor,4))
  )
print(
  xtable(tauladescform,
         caption = "Taula descriptiva dels log-retorns de les pèrdues",
         label = "tab:desc"),
  include.rownames = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity
)

# Funcions pels gràfics

plotpreu <- function(df,nom){
  
  ggplot(df,aes(x=date,y=close))+
    geom_line(color="steelblue")+
    labs(title=paste("Sèrie temporal del preu:",nom),
         x="Data",
         y="Preu")+
    theme_minimal()
  
}

plotretorns <- function(df,nom){
  
  ggplot(df,aes(x=date,y=r))+
    geom_line(color="darkred")+
    labs(title=paste("Log-retorns:",nom),
         x="Data",
         y="Log-retorn")+
    theme_minimal()
  
}

plothist <- function(df,nom){
  
  ggplot(df,aes(x=r))+
    geom_histogram(bins=100,fill="steelblue",alpha=0.7)+
    labs(title=paste("Distribució dels log-retorns:",nom),
         x="Log-retorn",
         y="Freqüència")+
    theme_minimal()
  
}

# qqplot

qq_crypto <- function(df, nom){
  
  ggplot(df, aes(sample = r)) +
    stat_qq(color="steelblue") +
    stat_qq_line(color="red") +
    labs(title=paste("QQ Plot vs Normal:", nom),
         x="Quantils teòrics",
         y="Quantils empírics") +
    theme_minimal()
  
}

# Descriptiva del preu

plotpreu(BTC,"BTC")
plotpreu(ETH,"ETH")
plotpreu(BNB,"BNB")
plotpreu(XRP,"XRP")
plotpreu(SOL,"SOL")

# Descriptiva dels logretorns negatius

plotretorns(BTC,"BTC")
plotretorns(ETH,"ETH")

plothist(BTC,"BTC")
plothist(ETH,"ETH")

# ACF sobre el preu

acf(BTC$r, main="ACF logretorns BTC")

acf(BTC$r^2, main="ACF logretorns^2 BTC")

acf(ETH$r, main="ACF logretorns ETH")

acf(ETH$r^2, main="ACF logretorns^2 ETH")