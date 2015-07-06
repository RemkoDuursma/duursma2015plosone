# Figures
source("R/functions.R")
library(plantecophys)


figure1 <- function(){
  
  par(xaxs="i", yaxs="i", cex.lab=1.3, mar=c(5,5,1,1), cex.axis=1,
      mfrow=c(1,2))
  
  ci <- seq(40, 1050, length=101)
  acirun <- Aci(Ci=ci, Vcmax=65, Jmax=130, Rd=1.5)
  
  with(acirun, {
    plot(Ci, Ac-Rd, type='l',col="red",lwd=2, ylim=c(0,40),
         xlab=expression(italic(C)[i]~~(ppm)),
         ylab=expression(italic(A)~~(mu*mol~m^-2~s^-1)),
                    xlim=c(0,1050))
    lines(Ci, Aj-Rd, col="blue", lwd=2)
    lines(Ci, ALEAF, lwd=2)
  })
  
  x <- plantecophys:::findCiTransition(Aci, Vcmax=65, Jmax=130, Rd=1.5)
  points(x, Aci(x, Vcmax=65, Jmax=130, Rd=1.5)$ALEAF, pch=21, cex=1.4, bg="white")
  
  legend("topleft", c(expression(A[j]),expression(A[c]), expression(A[n]), "Transition point"),
         lwd=c(2,2,2,-1), pch=c(-1,-1,-1,1), pt.cex=c(-1,-1,-1,1.4), col=c("blue","red","black", "black"),
         bty='n')
  
  ci <- seq(50, 800, length=101)
  plotlabel("(a)", "topleft")
  
  # PAnel b
  acirun <- Aci(Ci=ci)
  
  par(xaxs="i", yaxs="i", cex.lab=1.3, mar=c(5,5,1,1), cex.axis=1)
  with(acirun, plot(Ci, ALEAF, type='l',
                    ylim=c(0,20), xlim=c(0,800),
                    lwd=2,
                    xlab=expression(italic(C)[i]~~(ppm)),
                    ylab=expression(italic(A)[n]~~(mu*mol~m^-2~s^-1))))
  
  p <- Photosyn(VPD=2.5, g1=2)
  
  gc <- p$GS / 1.57
  abline(gc*p$Ca, -gc,  lwd=2, lty=5)
  
  points(p$Ci, p$ALEAF, pch=21, bg="white", cex=1.4)
  plotlabel("(b)", "topleft")
  
  legend("right", c("Demand function","Supply function","Operating point"),
         lwd=c(2,2,-1),pch=c(-1,-1,1), pt.cex=c(-1,-1,1.4),
         inset=0.01, cex=1.1, bty='n', lty=c(1,5,-1))
}






# example output fitaci
figure2 <- function(){
  
  
  f <- fitaci(acidata1)
  
  par(mar=c(5,5,1,1), cex.lab=1.3, cex.axis=1)
  plot(f)
  
}


figure3 <- function(){
  
  # Set range of leaf temperature
  tleafs <- seq(10,40,by=0.5)
  
  # Simulate.
  run1 <- Photosyn(Tleaf = tleafs, Vcmax=50, Jmax=2*50, g1=3.5)
  run2 <- Photosyn(Tleaf = tleafs, Vcmax=50, Jmax=1.5*50, g1=3.5)
  
  with(run1, plot(Tleaf, ALEAF, type='l', lwd=2,
                  ylim=c(0,15), xlim=c(0,45)))
  with(run2, lines(Tleaf, ALEAF, type='l', lty=5, lwd=2))
}


# FARAO
figure4 <- function(){
  vpds <- seq(0.5,3.5, by=0.5)
  lam <- 0.002
  f <- plantecophys:::OPTfun
  
  addcurve <- function(vpd, ...){
    curve(f(x, VPD=vpd)*10^6, from=80, to=360, add=TRUE, col="darkgrey")
    op <- FARAO(VPD=vpd)
    points(op$Ci, op$ALEAF - 0.002*op$ELEAF*10^3, ...)
  }
  
  Cols <- rev(grey(c(1:length(vpds))/length(vpds)))
  
  par(mfrow=c(1,2), mar=c(5,5,1,1), cex.lab=1.1)
  plot(1, type='n',        
       ylim=c(-1,10),
       xlim=c(0,400),
       xlab=expression(italic(C)[i]~~(ppm)),
       ylab=expression(A-lambda*E~~(mu*mol~m^-2~s^-1)))
  for(i in 1:length(vpds))addcurve(vpds[i], pch=21, bg=Cols[i], cex=1.2)
  plotlabel("(a)","bottomleft")
  legend("topleft", c(expression(italic(D) == 0.5*kPa),expression(italic(D) == 3.5*kPa)),
         pch=21, pt.bg=c(Cols[1], Cols[length(Cols)]), bty='n', pt.cex=1.2, cex=0.8)
  
  opt <- FARAO(VPD=vpds)
  with(opt, plot(VPD, GS, xlim=c(0,max(vpds)+0.5),
                 pch=21, bg=Cols, cex=1.2,
                 ylim=c(0,max(GS)+0.01), type='o', 
                 xlab=expression(italic(D)~~(kPa)),
                 ylab=expression(italic(g)[s]~~(mol~m^-2~s^-1))
                 ))
  plotlabel("(b)","bottomleft")
}
  