library(mclust)
library(data.table)
library(mvtnorm)

plotparlogmixtura <- function(r1, r2, nom1, nom2, fit3,
                               fitxer = "parells", nbins = 100, freq = "") {
  
  mu    <- fit3$parameters$mean
  sigma <- fit3$parameters$variance$sigma
  ord   <- order(mu[1,] + mu[2,])
  noms3 <- c("neg-neg", "central", "pos-pos")
  cols3 <- c("tomato", "gray80", "steelblue")
  
  casos <- list(
    list(-r1[r1<0 & r2>0],  r2[r1<0 & r2>0],  paste0(nom1,"-"), paste0(nom2,"+"), TRUE,  FALSE, c(-1,  1)),
    list( r1[r1>0 & r2>0],  r2[r1>0 & r2>0],  paste0(nom1,"+"), paste0(nom2,"+"), FALSE, FALSE, c( 1,  1)),
    list(-r1[r1<0 & r2<0], -r2[r1<0 & r2<0],  paste0(nom1,"-"), paste0(nom2,"-"), TRUE,  TRUE,  c(-1, -1)),
    list( r1[r1>0 & r2<0], -r2[r1>0 & r2<0],  paste0(nom1,"+"), paste0(nom2,"-"), FALSE, TRUE,  c( 1, -1))
  )
  
  mats  <- vector("list", 4)
  bxs   <- vector("list", 4)
  bys   <- vector("list", 4)
  valid <- logical(4)
  
  for (i in seq_along(casos)) {
    cas <- casos[[i]]
    x <- cas[[1]]; y <- cas[[2]]
    x <- x[is.finite(x) & x > 0]
    y <- y[is.finite(y) & y > 0]
    if (length(x) < 2 || length(y) < 2) next
    valid[i] <- TRUE
    
    bx <- exp(seq(log(min(x)), log(max(x)), length.out = nbins + 1))
    by <- exp(seq(log(min(y)), log(max(y)), length.out = nbins + 1))
    m  <- matrix(0L, nbins, nbins)
    xi <- pmax(1, pmin(nbins, findInterval(x, bx, rightmost.closed = TRUE)))
    yi <- pmax(1, pmin(nbins, findInterval(y, by, rightmost.closed = TRUE)))
    for (j in seq_along(xi)) m[xi[j], yi[j]] <- m[xi[j], yi[j]] + 1L
    
    mats[[i]] <- m; bxs[[i]] <- bx; bys[[i]] <- by
  }
  
  zlim <- range(unlist(lapply(mats[valid], log1p)), na.rm = TRUE)
  cols <- hcl.colors(256, "YlOrRd", rev = TRUE)
  
  png(paste0(fitxer, "_log_mixtura.png"), width = 1650, height = 1400, res = 150)
  layout(
    matrix(c(1, 2, 5,
             3, 4, 5), nrow = 2, ncol = 3, byrow = TRUE),
    widths = c(1, 1, 0.12)
  )
  
  for (i in seq_along(casos)) {
    par(mar = c(4, 4, 3, 1))
    cas <- casos[[i]]
    
    if (!valid[i]) { plot.new(); next }
    
    bx   <- bxs[[i]]; by   <- bys[[i]]; m <- mats[[i]]
    xinv <- cas[[5]];  yinv <- cas[[6]]
    sx   <- cas[[7]][1]; sy <- cas[[7]][2]
    
    lbx <- log(bx); lby <- log(by)
    xlimuse <- if (xinv) rev(range(lbx)) else range(lbx)
    ylimuse <- if (yinv) rev(range(lby)) else range(lby)
    atx <- pretty(lbx); aty <- pretty(lby)
    
    image(lbx, lby, log1p(m),
          col  = cols, zlim = zlim,
          xlim = xlimuse, ylim = ylimuse,
          main = paste0(cas[[3]], " vs ", cas[[4]],
                        if (nchar(freq)) paste0("  [", freq, "]")),
          xlab = cas[[3]], ylab = cas[[4]],
          axes = FALSE)
    axis(1, at = atx, labels = formatC(exp(atx), format = "e", digits = 1))
    axis(2, at = aty, labels = formatC(exp(aty), format = "e", digits = 1))
    box()
    
    gxlog <- seq(range(lbx)[1], range(lbx)[2], length.out = 120)
    gylog <- seq(range(lby)[1], range(lby)[2], length.out = 120)
    grlog <- as.matrix(expand.grid(gxlog, gylog))
    grorig <- cbind(sx * exp(grlog[, 1]),
                     sy * exp(grlog[, 2]))
    
    for (k in seq_along(ord)) {
      muk    <- mu[, ord[k]]
      sigmak <- sigma[,, ord[k]]
      dens    <- dmvnorm(grorig, muk, sigmak)
      densm  <- matrix(dens, length(gxlog), length(gylog))
      lev     <- quantile(dens[dens > 0], c(0.70, 0.90, 0.97))
      contour(gxlog, gylog, densm,
              add = TRUE, col = cols3[k], lwd = 1.5,
              levels = lev, drawlabels = FALSE)
    }
    
    for (k in seq_along(ord)) {
      muk   <- mu[, ord[k]]
      mxlog <- sx * muk[1]
      mylog <- sy * muk[2]
      if (mxlog > 0 && mylog > 0) {
        points(log(mxlog), log(mylog), pch = 3, cex = 2, lwd = 2, col = cols3[k])
      }
    }
    
    if (i == min(which(valid))) {
      legend("topright", legend = noms3, col = cols3,
             lty = 1, lwd = 2, bty = "n", cex = 0.75)
    }
  }
  
  par(mar = c(4, 0.5, 3, 3.5))
  cby <- seq(zlim[1], zlim[2], length.out = 256)
  image(x = 1, y = cby, z = matrix(cby, nrow = 1),
        col = cols, axes = FALSE, xlab = "", ylab = "")
  axis(4, las = 1, cex.axis = 0.75)
  mtext("log(1+n)", side = 4, line = 2.8, cex = 0.8)
  box()
  
  title(main = paste0(nom1, " vs ", nom2, " — Mixtura 3 normals (escala log)"),
        outer = TRUE, line = -1.5, cex.main = 1.1)
  
  dev.off()
}


parells <- combn(noms, 2, simplify = FALSE)

for (p in parells) {
  n1 <- p[1]; n2 <- p[2]
  cat(n1, "vs", n2, "\n")
  
  # Dades
  d1 <- llista1m[[n1]][, .(t = data, r1 = ret)]
  d2 <- llista1m[[n2]][, .(t = data, r2 = ret)]
  dm <- merge(d1, d2, by = "t")
  dm <- dm[is.finite(r1) & is.finite(r2) & r1 != 0 & r2 != 0]
  X  <- as.matrix(dm[, .(r1, r2)])
  
  # Ajust G=1,2,3 lliure (per BIC de referència)
  fits <- lapply(1:3, function(g) Mclust(X, G = g, modelNames = "VVV", verbose = FALSE))
  bics <- sapply(fits, function(f) f$bic)
  cat("BIC G=1:", round(bics[1]), " G=2:", round(bics[2]), " G=3:", round(bics[3]), "\n")
  cat("G=3 preferit:", bics[3] > bics[1], "\n")
  
  fitA <- fits[[3]]
  
  q75 <- quantile(abs(X), 0.75)
  muinitB <- matrix(c(0, 0, q75, q75, -q75, -q75), nrow = 2)
  distsB   <- sapply(1:3, function(k) rowSums((X - rep(muinitB[, k], each = nrow(X)))^2))
  zinitB  <- matrix(0, nrow(X), 3)
  zinitB[cbind(1:nrow(X), max.col(-distsB))] <- 1
  fitB <- Mclust(X, G = 3, modelNames = "VVV", verbose = FALSE,
                 initialization = list(z = zinitB))
  
  pp    <- X[X[, 1] >  0 & X[, 2] >  0, , drop = FALSE]
  nn    <- X[X[, 1] <  0 & X[, 2] <  0, , drop = FALSE]
  mupp <- if (nrow(pp) > 0) colMeans(pp) else c( q75,  q75)
  munn <- if (nrow(nn) > 0) colMeans(nn) else c(-q75, -q75)
  muinitC <- matrix(c(0, 0, mupp, munn), nrow = 2)
  distsC   <- sapply(1:3, function(k) rowSums((X - rep(muinitC[, k], each = nrow(X)))^2))
  zinitC  <- matrix(0, nrow(X), 3)
  zinitC[cbind(1:nrow(X), max.col(-distsC))] <- 1
  fitC <- Mclust(X, G = 3, modelNames = "VVV", verbose = FALSE,
                 initialization = list(z = zinitC))
  
  cat("\n Comparació models \n")
  cat(sprintf("BIC  A (lliure):         %12.0f\n", fitA$bic))
  cat(sprintf("BIC  B (init q75):       %12.0f\n", fitB$bic))
  cat(sprintf("BIC  C (init quadrants): %12.0f\n", fitC$bic))
  cat(sprintf("Millor model: %s\n",
              c("A", "B", "C")[which.max(c(fitA$bic, fitB$bic, fitC$bic))]))
  
  for (tag in c("A", "B", "C")) {
    f    <- get(paste0("fit", tag))
    muf  <- f$parameters$mean
    ordf <- order(muf[1,] + muf[2,])
    cat(sprintf("\n Model %s (log-lik=%.0f)\n", tag, f$loglik))
    cat("  Proporcions:", round(f$parameters$pro[ordf], 4), "\n")
    for (k in 1:3) {
      lab <- c("neg-neg", "central", "pos-pos")[k]
      cat(sprintf("  %s  mu=(%+.2e, %+.2e)  tr(sigma)=%.2e\n",
                  lab,
                  muf[1, ordf[k]], muf[2, ordf[k]],
                  sum(diag(f$parameters$variance$sigma[,, ordf[k]]))))
    }
  }
  
  fit  <- get(paste0("fit", c("A", "B", "C")[which.max(c(fitA$bic, fitB$bic, fitC$bic))]))
  comp <- fit$classification
  mu   <- fit$parameters$mean
  ord  <- order(mu[1,] + mu[2,])
  noms3 <- c("neg-neg", "central", "pos-pos")
  cols3 <- c("tomato", "gray80", "steelblue")
  pref  <- paste0(n1, "_", n2, "_1min")
  
  png(paste0("mixtura_contorn_", pref, ".png"), width = 1000, height = 1000, res = 150)
  bx <- seq(min(X[, 1]), max(X[, 1]), length.out = 101)
  by <- seq(min(X[, 2]), max(X[, 2]), length.out = 101)
  m  <- matrix(0L, 100, 100)
  xi <- pmax(1, pmin(100, findInterval(X[, 1], bx, rightmost.closed = TRUE)))
  yi <- pmax(1, pmin(100, findInterval(X[, 2], by, rightmost.closed = TRUE)))
  for (i in seq_along(xi)) m[xi[i], yi[i]] <- m[xi[i], yi[i]] + 1L
  image(bx, by, log1p(m), col = hcl.colors(100, "YlOrRd", rev = TRUE),
        main = paste0(n1, " vs ", n2, " (1min) — Mixtura 3 normals"),
        xlab = paste0("ret ", n1), ylab = paste0("ret ", n2))
  gx <- seq(min(X[, 1]), max(X[, 1]), length.out = 150)
  gy <- seq(min(X[, 2]), max(X[, 2]), length.out = 150)
  gr <- as.matrix(expand.grid(gx, gy))
  for (k in seq_along(ord)) {
    dens <- matrix(dmvnorm(gr, mu[, ord[k]], fit$parameters$variance$sigma[,, ord[k]]),
                   length(gx), length(gy))
    contour(gx, gy, dens, add = TRUE, col = cols3[k], lwd = 1.5,
            levels = quantile(dens[dens > 0], c(0.7, 0.9, 0.97)), drawlabels = FALSE)
  }
  abline(h = 0, v = 0, lty = 2, col = "white", lwd = 0.8)
  points(t(mu[, ord]), pch = 3, cex = 2, lwd = 2, col = cols3)
  points(0, 0, pch = 4, cex = 2, lwd = 2, col = "yellow")
  legend("topright", noms3, col = cols3, lty = 1, lwd = 2, bty = "n", cex = 0.8)
  dev.off()
  
  png(paste0("mixtura_posteriors_", pref, ".png"), width = 1400, height = 500, res = 150)
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  for (k in seq_along(ord)) {
    prob <- fit$z[, ord[k]]
    idx  <- order(prob)
    plot(X[idx, 1], X[idx, 2], pch = ".",
         col = colorRampPalette(c("gray90", "blue", "darkblue"))(100)[pmax(1, ceiling(prob[idx] * 100))],
         main = paste0("P(", noms3[k], ")"),
         xlab = paste0("ret ", n1), ylab = paste0("ret ", n2))
  }
  dev.off()
  
  cr    <- match(comp, ord)
  trans <- table(cr[-length(cr)], cr[-1])
  tp    <- round(trans / rowSums(trans), 3)
  rownames(tp) <- colnames(tp) <- noms3
  cat("Matriu de transicions:\n"); print(tp)
  
  png(paste0("mixtura_transicions_", pref, ".png"), width = 600, height = 600, res = 150)
  image(1:3, 1:3, t(tp[3:1, ]), col = hcl.colors(20, "Blues", rev = TRUE),
        axes = FALSE, main = paste0("Transicions ", n1, " vs ", n2),
        xlab = "cap a", ylab = "des de")
  axis(1, 1:3, noms3, cex.axis = 0.8)
  axis(2, 1:3, rev(noms3), cex.axis = 0.8)
  for (i in 1:3) for (j in 1:3) text(j, 4 - i, tp[i, j], cex = 0.9)
  dev.off()
  
  plotparlogmixtura(
    r1     = dm$r1,
    r2     = dm$r2,
    nom1   = n1,
    nom2   = n2,
    fit3   = fit,
    fitxer = paste0("mixtura_", pref),
    freq   = "1min"
  )
}