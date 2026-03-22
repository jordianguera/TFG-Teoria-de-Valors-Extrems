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

dfbind[, datahora := as.POSIXct(cut(date, "1 hour"))]
dfbind[, datadia := as.Date(date)]

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

guardarplot <- function(plot, fitxer, nom) {
  nom_fitxer <- paste0(fitxer, nom, ".png")
  ggsave(nom_fitxer, plot = plot, width = 8, height = 6, dpi = 300)
}

plotpreu <- function(df, nom){
  p <- ggplot(df, aes(x = date, y = close)) +
    geom_line(color = "steelblue") +
    labs(title = paste("Sèrie temporal del preu:", nom),
         x = "Data",
         y = "Preu") +
    theme_minimal()
  guardarplot(p,"SerieCompleta_", nom)
}


plotretorns <- function(df, nom){
  p <- ggplot(df, aes(x = date, y = r)) +
    geom_line(color = "darkred") +
    labs(title = paste("Log-retorns:", nom),
         x = "Data",
         y = "Log-retorn") +
    theme_minimal()
  guardarplot(p,"Logretorns_", nom)
}

plotperduesminut <- function(df, nom){
  p <- ggplot(df[crypto==nom & !is.na(loss)], aes(x=date, y=loss)) +
    geom_line(color="darkred") +
    labs(title=paste("Log-pèrdues minut:", nom),
         x="Temps",
         y="Pèrdua") +
    theme_minimal()
  
  guardarplot(p, "PerduesMinut_", nom)
}

plotperdueshora <- function(df, nom){
  dades <- df[, .(
    perdua = sum(pmax(-r,0), na.rm=TRUE)
  ), by=.(crypto, datahora)]
  
  p <- ggplot(dades[crypto==nom], aes(x=datahora, y=perdua)) +
    geom_line(color="darkred") +
    labs(title=paste("Log-pèrdues per hora:", nom),
         x="Temps",
         y="Pèrdua") +
    theme_minimal()
  
  guardarplot(p, "PerduesHora_", nom)
}

plotperduesdia <- function(df, nom){
  dades <- df[, .(
    perdua = sum(pmax(-r,0), na.rm=TRUE)
  ), by=.(crypto, datadia)]
  
  p <- ggplot(dades[crypto==nom], aes(x=datadia, y=perdua)) +
    geom_line(color="darkred") +
    labs(title=paste("Log-pèrdues diàries:", nom),
         x="Temps",
         y="Pèrdua") +
    theme_minimal()
  
  guardarplot(p, "PerduesDia_", nom)
}

plotcomparacio <- function(df){
  dades <- df[, .(
    perdua = sum(pmax(-r,0), na.rm=TRUE)
  ), by=.(crypto, datadia)]
  
  p <- ggplot(dades, aes(x=datadia, y=perdua, color=crypto)) +
    geom_line() +
    labs(title="Comparació de pèrdues diàries",
         x="Temps",
         y="Pèrdua") +
    theme_minimal()
  
  guardarplot(p, "ComparacioPerdues_", "Totes")
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

# Descriptiva dels retorns

plotretorns(BTC,"BTC")
plotretorns(ETH,"ETH")
plotretorns(BNB,"BNB")
plotretorns(XRP,"XRP")
plotretorns(SOL,"SOL")

# Descriptiva dels logretorns negatius

criptos <- c("BTC","ETH","BNB","XRP","SOL")

for (c in criptos){
  plotperduesminut(dfbind, c)
  plotperdueshora(dfbind, c)
  plotperduesdia(dfbind, c)
}

# punt més extrem (retorn més negatiu)
extremBTC <- BTC[which.min(r)]

t0 <- extremBTC$date

# finestra +- 6 hores

finestra <- dfbind[
  date >= (t0 - 6*3600) & date <= (t0 + 6*3600)
]

# Funcio finestra

plotvelesfinestra <- function(df, nom){
  dades <- df[crypto == nom]
  
  p <- ggplot(dades, aes(x=date)) +
    geom_segment(aes(y=min, yend=max, xend=date), color="black") +
    geom_rect(aes(
      xmin=date-30,
      xmax=date+30,
      ymin=pmin(open,close),
      ymax=pmax(open,close),
      fill=close>open
    )) +
    scale_fill_manual(values=c("red","green")) +
    geom_vline(xintercept = as.numeric(t0), linetype="dashed")+
    labs(title=paste("Candlestick finestra d'estrès:", nom),
         subtitle=paste("Xoc BTC a:", format(t0)),
         x="Temps",
         y="Preu") +
    theme_minimal() +
    theme(legend.position="none")
  
  guardarplot(p, "CandlesFinestra_", nom)
}


criptos <- c("BTC","ETH","BNB","XRP","SOL")

for (c in criptos){
  plotvelesfinestra(finestra, c)
}

plothist(BTC,"BTC")
plothist(ETH,"ETH")

# ACF sobre el preu

acf(BTC$r, main="ACF logretorns BTC")

acf(BTC$r^2, main="ACF logretorns^2 BTC")

acf(ETH$r, main="ACF logretorns ETH")

acf(ETH$r^2, main="ACF logretorns^2 ETH")