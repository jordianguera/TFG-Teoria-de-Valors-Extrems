library(mclust)
library(data.table)
library(mvtnorm)

plotparlogquadrants <- function(r1, r2, nom1, nom2, fits_q,
                                fitxer = "parells", nbins = 100, freq = "") {
  
  cols4 <- c("orange", "steelblue", "tomato", "purple")
  
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
    x <- x[is.finite(x) & x > 0]; y <- y[is.finite(y) & y > 0]
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
  
  png(paste0(fitxer, "_log_quadrants.png"), width = 1650, height = 1400, res = 150)
  layout(matrix(c(1, 2, 5, 3, 4, 5), nrow = 2, ncol = 3, byrow = TRUE),
         widths = c(1, 1, 0.12))
  
  for (i in seq_along(casos)) {
    par(mar = c(4, 4, 3, 1))
    cas <- casos[[i]]
    if (!valid[i]) { plot.new(); next }
    
    bx <- bxs[[i]]; by <- bys[[i]]; m <- mats[[i]]
    sx <- cas[[7]][1]; sy <- cas[[7]][2]
    lbx <- log(bx); lby <- log(by)
    xlimuse <- if (cas[[5]]) rev(range(lbx)) else range(lbx)
    ylimuse <- if (cas[[6]]) rev(range(lby)) else range(lby)
    atx <- pretty(lbx); aty <- pretty(lby)
    
    image(lbx, lby, log1p(m),
          col = cols, zlim = zlim, xlim = xlimuse, ylim = ylimuse,
          main = paste0(cas[[3]], " vs ", cas[[4]],
                        if (nchar(freq)) paste0("  [", freq, "]")),
          xlab = cas[[3]], ylab = cas[[4]], axes = FALSE)
    axis(1, at = atx, labels = formatC(exp(atx), format = "e", digits = 1))
    axis(2, at = aty, labels = formatC(exp(aty), format = "e", digits = 1))
    box()
    
    fq <- fits_q[[i]]
    if (!is.null(fq)) {
      muk    <- fq$parameters$mean
      sigmak <- fq$parameters$variance$sigma[,, 1]   # <-- afegir [,,1]
      
      gxlog <- seq(range(lbx)[1], range(lbx)[2], length.out = 120)
      gylog <- seq(range(lby)[1], range(lby)[2], length.out = 120)
      grlog <- as.matrix(expand.grid(gxlog, gylog))
      grorig <- cbind(sx * exp(grlog[, 1]), sy * exp(grlog[, 2]))
      
      dens  <- dmvnorm(grorig, muk, sigmak)
      densm <- matrix(dens, length(gxlog), length(gylog))
      lev   <- quantile(dens[dens > 0], c(0.70, 0.90, 0.97))
      contour(gxlog, gylog, densm, add = TRUE,
              col = cols4[i], lwd = 1.5, levels = lev, drawlabels = FALSE)
      
      mxlog <- sx * muk[1]; mylog <- sy * muk[2]
      if (mxlog > 0 && mylog > 0)
        points(log(mxlog), log(mylog), pch = 3, cex = 2, lwd = 2, col = cols4[i])
    }
  }
  
  par(mar = c(4, 0.5, 3, 3.5))
  cby <- seq(zlim[1], zlim[2], length.out = 256)
  image(x = 1, y = cby, z = matrix(cby, nrow = 1),
        col = cols, axes = FALSE, xlab = "", ylab = "")
  axis(4, las = 1, cex.axis = 0.75)
  mtext("log(1+n)", side = 4, line = 2.8, cex = 0.8)
  box()
  
  title(main = paste0(nom1, " vs ", nom2, ": 1 normal per quadrant (escala log)"),
        outer = TRUE, line = -1.5, cex.main = 1.1)
  dev.off()
}


parells <- combn(noms, 2, simplify = FALSE)

for (p in parells) {
  n1 <- p[1]; n2 <- p[2]
  cat(n1, "vs", n2, "\n")
  
  d1 <- llista1m[[n1]][, .(t = data, r1 = ret)]
  d2 <- llista1m[[n2]][, .(t = data, r2 = ret)]
  dm <- merge(d1, d2, by = "t")
  dm <- dm[is.finite(r1) & is.finite(r2) & r1 != 0 & r2 != 0]
  X  <- as.matrix(dm[, .(r1, r2)])
  
  # Ordre coincideix amb casos de plotparlogquadrants: TL(-+), TR(++), BL(--), BR(+-)
  masks <- list(
    X[,1] < 0 & X[,2] > 0,
    X[,1] > 0 & X[,2] > 0,
    X[,1] < 0 & X[,2] < 0,
    X[,1] > 0 & X[,2] < 0
  )
  noms_q <- c("neg-pos", "pos-pos", "neg-neg", "pos-neg")
  
  fits_q <- lapply(seq_along(masks), function(i) {
    Xq <- X[masks[[i]], , drop = FALSE]
    if (nrow(Xq) < 10) return(NULL)
    Mclust(Xq, G = 1, modelNames = "VVV", verbose = FALSE)
  })
  
  cat("\nResultats per quadrant:\n")
  for (i in seq_along(fits_q)) {
    fq <- fits_q[[i]]
    if (is.null(fq)) { cat(noms_q[i], ": insuficient\n"); next }
    cat(sprintf("  %s  n=%d  mu=(%+.2e, %+.2e)  tr(Sigma)=%.2e\n",
                noms_q[i], sum(masks[[i]]),
                fq$parameters$mean[1], fq$parameters$mean[2],
                sum(diag(fq$parameters$variance$sigma[,, 1]))))
  }
  
  pref <- paste0(n1, "_", n2, "_1min")
  
  plotparlogquadrants(
    r1     = dm$r1, r2 = dm$r2,
    nom1   = n1,    nom2 = n2,
    fits_q = fits_q,
    fitxer = paste0("quadrants_", pref),
    freq   = "1min"
  )
}