library(evir)
library(ercv)
library(ismev)

dfperdues <- lapply(llista1h, function(df) df$perd)

# 1. Mean Excess Plot
meplot(dfperdues$BTC[dfperdues$BTC>0])

# fins a 0.05 la corba és suau però lleugerament corbada
# a partir de 0.05 és relativament lineal

length(dfperdues$BTC[dfperdues$BTC>0])

length(dfperdues$BTC[dfperdues$BTC>0.04])

length(dfperdues$BTC[dfperdues$BTC>0.045])

length(dfperdues$BTC[dfperdues$BTC>0.05])


# ETH

meplot(dfperdues$ETH[dfperdues$ETH>0])

