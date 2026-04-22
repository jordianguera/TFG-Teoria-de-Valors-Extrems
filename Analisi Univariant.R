library(evir)
library(ercv)
library(ismev)
library(extRemes)
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
mrlplot(perduesBTC1m)
tcplot(perduesBTC1m)
cvplot(perduesBTC1m)

# 1h
meplot(perduesBTC1h)
hill(perduesBTC1h)
mrlplot(perduesBTC1h)
tcplot(perduesBTC1h)
cvplot(perduesBTC1h)

# 1d
meplot(perduesBTC1d)
hill(perduesBTC1d)
mrlplot(perduesBTC1d)
tcplot(perduesBTC1d)
cvplot(perduesBTC1d)



