# Figures
source("R/functions.R")
library(plantecophys)

figure1 <- function(){
  
  
  f <- fitaci(acidata1)
  
  par(mar=c(5,5,1,1), cex.lab=1.3, cex.axis=1)
  plot(f)
  
}
to.pdf(figure1(), filename="manuscript/figures/figure1.pdf")



# Sharkey figure
figure2 <- function(){
  ci <- seq(50, 800, length=101)
  
  acirun <- Aci(Ci=ci)
  
  par(xaxs="i", yaxs="i", cex.lab=1.3, mar=c(5,5,1,1), cex.axis=1)
  with(acirun, plot(Ci, ALEAF, type='l',
                    ylim=c(0,20), xlim=c(0,800),
                    col="blue",lwd=2,
                    xlab=expression(italic(C)[i]~~(ppm)),
                    ylab=expression(italic(A)[leaf]~~(mu*mol~m^-2~s^-1))))
  
  p <- Photosyn(VPD=2.5, g1=2)
  
  gc <- p$GS / 1.6
  abline(gc*p$Ca, -gc, col="forestgreen", lwd=2)
  
  points(p$Ci, p$ALEAF, pch=19, col="red", cex=1.1)
  
  legend("right", c(expression("Demand curve: "~A==f(italic(C)[i])),
                          "Supply curve: "~A==g[CO2]*(italic(C)[a]-italic(C)[i]),
                          "Operating point"),
         lwd=c(2,2,-1),pch=c(-1,-1,19), pt.cex=c(-1,-1,1.1),
         col=c("blue","forestgreen","red"), inset=0.01, cex=1.1, bty='n')
}
to.pdf(figure2(), filename="manuscript/figures/figure2.pdf")

  