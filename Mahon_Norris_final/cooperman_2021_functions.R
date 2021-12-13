# Code Adapted by Alicia Cooperman


create.rd.plot.pdf <- function (z, y, state, cutpt.sharp=0, data=data,interval=5,smoothing=.2, bin.size=.03, color="black",titles=paste0("Loess Plot -- ",state),...){
  d <- ifelse(z >= cutpt.sharp, 1, 0) ## create d for treatment var.
  rd.frame <- subset(data.frame(z,d,y),abs(z)<interval & !is.na(y) & !is.na(z))
  rd.frame <- rd.frame[with(rd.frame, order(z)), ]
  with(rd.frame,{
    numbelow <- length(seq(min(z), cutpt.sharp, bin.size))
    numabove <- length(seq(cutpt.sharp, max(z), bin.size))
    b1 <- cut(z[z < cutpt.sharp], numbelow, labels=FALSE)
    b2 <- 1000 + cut(z[z >= cutpt.sharp], numabove, labels=FALSE)
    dat.below <- cbind(b1, z[z < cutpt.sharp], y[z < cutpt.sharp])
    dat.above <- cbind(b2, z[z >= cutpt.sharp], y[z >= cutpt.sharp])
    dat.binned <- as.data.frame(rbind(dat.below, dat.above))
    names(dat.binned) <- c("bin","z","y")
    dat.binned <- dat.binned[with(dat.binned, order(z)), ]
    y.mean.binned <- tapply(dat.binned$y, dat.binned$bin, mean)
    z.mean.binned <- tapply(dat.binned$z, dat.binned$bin, mean)
    par(mar = c(5,5,4,2) + 0.05) 
    radius <- sqrt(tapply(dat.binned$z, dat.binned$bin, length))
    symbols(z.mean.binned, y.mean.binned, circles=radius, inches=0.25,
            ylim=c(0,1.1), xlim=c(-2.5, 2.5),ylab="",xlab="", cex.axis=3)
    title(titles, cex.main=3)
    title(ylab="Prob. Drought Dec.",xlab="SPI", cex.lab=3)
    loess.pos <- loess(y[d==1] ~ z[d==1], span=smoothing, na.action="na.omit")
    loess.neg <- loess(y[d==0] ~ z[d==0], span=smoothing, na.action="na.omit")
    loess.all <- loess(y~z,span=smoothing, na.action="na.omit")
    lines(z, predict(loess.all), col=color, lwd=4)
    abline(v=0, col="red", lwd=3)   
  })
}


create.rd.lines.incwin <- function (z, y, state, cutpt.sharp=0, data=data,interval=5,smoothing=.2, bin.size=.03, color="black", ylab="Probability of Re-Election", lty=1, ...){
  d <- ifelse(z >= cutpt.sharp, 1, 0) ## create d for treatment var.
  rd.frame <- subset(data.frame(z,d,y),abs(z)<interval & !is.na(y) & !is.na(z))
  rd.frame <- rd.frame[with(rd.frame, order(z)), ]
  with(rd.frame,{
    numbelow <- length(seq(min(z), cutpt.sharp, bin.size))
    numabove <- length(seq(cutpt.sharp, max(z), bin.size))
    b1 <- cut(z[z < cutpt.sharp], numbelow, labels=FALSE)
    b2 <- 1000 + cut(z[z >= cutpt.sharp], numabove, labels=FALSE)
    dat.below <- cbind(b1, z[z < cutpt.sharp], y[z < cutpt.sharp])
    dat.above <- cbind(b2, z[z >= cutpt.sharp], y[z >= cutpt.sharp])
    dat.binned <- as.data.frame(rbind(dat.below, dat.above))
    names(dat.binned) <- c("bin","z","y")
    dat.binned <- dat.binned[with(dat.binned, order(z)), ]
    y.mean.binned <- tapply(dat.binned$y, dat.binned$bin, mean)
    z.mean.binned <- tapply(dat.binned$z, dat.binned$bin, mean)
    par(mar = c(5,5,4,2) + 0.05) 
    radius <- sqrt(tapply(dat.binned$z, dat.binned$bin, length))
    symbols(z.mean.binned, y.mean.binned, circles=radius, inches=0.25, add=T, lty=lty)
    loess.all <- loess(y~z,span=smoothing, na.action="na.omit")
    lines(z, predict(loess.all), col=color, lwd=3, lty=lty)
    abline(v=0, col="red")   
  })
}


