library(mclust)
library(mvtnorm)

parells <- combn(noms, 2, simplify = FALSE)

for (p in parells) {
  n1 <- p[1]; n2 <- p[2]
  cat(n1, "vs", n2, "\n")
  
  # dades
  d1 <- llista1m[[n1]][, .(t = data, r1 = ret)]
  d2 <- llista1m[[n2]][, .(t = data, r2 = ret)]
  dm <- merge(d1, d2, by = "t")
  dm <- dm[is.finite(r1) & is.finite(r2) & r1 != 0 & r2 != 0]
  X  <- as.matrix(dm[, .(r1, r2)])
  
  # ajust G=1,2,3
  fits <- lapply(1:3, function(g) Mclust(X, G = g, modelNames = "VVV", verbose = FALSE))
  bics <- sapply(fits, function(f) f$bic)
  cat("BIC G=1:", round(bics[1]), " G=2:", round(bics[2]), " G=3:", round(bics[3]), "\n")
  cat("G=3 preferit:", bics[3] > bics[1], "\n")
  
  fit   <- fits[[3]]
  comp  <- fit$classification
  mu    <- fit$parameters$mean
  ord   <- order(mu[1,] + mu[2,])
  noms3 <- c("neg-neg","central","pos-pos")
  cols3 <- c("tomato","gray80","steelblue")
  pref  <- paste0(n1, "_", n2, "_1min")
  
  
  
  # gràfic 1: heatmap + contorns
  png(paste0("mixtura_contorn_", pref, ".png"), width=1000, height=1000, res=150)
  bx <- seq(min(X[,1]), max(X[,1]), length.out=101)
  by <- seq(min(X[,2]), max(X[,2]), length.out=101)
  m  <- matrix(0L, 100, 100)
  xi <- pmax(1, pmin(100, findInterval(X[,1], bx, rightmost.closed=TRUE)))
  yi <- pmax(1, pmin(100, findInterval(X[,2], by, rightmost.closed=TRUE)))
  for (i in seq_along(xi)) m[xi[i], yi[i]] <- m[xi[i], yi[i]] + 1L
  image(bx, by, log1p(m), col=hcl.colors(100,"YlOrRd",rev=TRUE),
        main=paste0(n1," vs ",n2," (1min) — Mixtura 3 normals"),
        xlab=paste0("ret ",n1), ylab=paste0("ret ",n2))
  gx <- seq(min(X[,1]), max(X[,1]), length.out=150)
  gy <- seq(min(X[,2]), max(X[,2]), length.out=150)
  gr <- as.matrix(expand.grid(gx, gy))
  for (k in seq_along(ord)) {
    dens <- matrix(dmvnorm(gr, mu[,ord[k]], fit$parameters$variance$sigma[,,ord[k]]),
                   length(gx), length(gy))
    contour(gx, gy, dens, add=TRUE, col=cols3[k], lwd=1.5,
            levels=quantile(dens[dens>0], c(0.7,0.9,0.97)), drawlabels=FALSE)
  }
  abline(h=0, v=0, lty=2, col="white", lwd=0.8)
  points(t(mu[,ord]), pch=3, cex=2, lwd=2, col=cols3)
  points(0, 0, pch=4, cex=2, lwd=2, col="yellow")
  legend("topright", noms3, col=cols3, lty=1, lwd=2, bty="n", cex=0.8)
  dev.off()
  
  # gràfic 2: probabilitats posteriors
  png(paste0("mixtura_posteriors_", pref, ".png"), width=1400, height=500, res=150)
  par(mfrow=c(1,3), mar=c(4,4,3,1))
  for (k in seq_along(ord)) {
    prob <- fit$z[, ord[k]]
    idx  <- order(prob)
    plot(X[idx,1], X[idx,2], pch=".",
         col=colorRampPalette(c("gray90","blue","darkblue"))(100)[pmax(1,ceiling(prob[idx]*100))],
         main=paste0("P(",noms3[k],")"), xlab=paste0("ret ",n1), ylab=paste0("ret ",n2))
  }
  dev.off()
  
  # gràfic 3: matriu de transicions
  cr    <- match(comp, ord)
  trans <- table(cr[-length(cr)], cr[-1])
  tp    <- round(trans / rowSums(trans), 3)
  rownames(tp) <- colnames(tp) <- noms3
  cat("Matriu de transicions:\n"); print(tp)
  
  png(paste0("mixtura_transicions_", pref, ".png"), width=600, height=600, res=150)
  image(1:3, 1:3, t(tp[3:1,]), col=hcl.colors(20,"Blues",rev=TRUE),
        axes=FALSE, main=paste0("Transicions ",n1," vs ",n2), xlab="cap a", ylab="des de")
  axis(1, 1:3, noms3, cex.axis=0.8); axis(2, 1:3, rev(noms3), cex.axis=0.8)
  for (i in 1:3) for (j in 1:3) text(j, 4-i, tp[i,j], cex=0.9)
  dev.off()
}