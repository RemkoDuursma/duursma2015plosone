# Figures
source("R/functions.R")
library(plantecophys)


figure1 <- function(){
  
  par(xaxs="i", yaxs="i", cex.lab=1.3, mar=c(4,4,1,1), cex.axis=1,
      mfrow=c(1,2), mgp=c(2,0.5,0), las=1, tcl=0.2)
  
  ci <- seq(40, 1050, length=101)
  acirun <- Aci(Ci=ci, Vcmax=65, Jmax=130, Rd=1.5)
  
  with(acirun, {
    plot(Ci, Ac-Rd, type='l',col="red",lwd=2, ylim=c(0,40),
         xlab=expression(italic(C)[i]~~(ppm)),
         ylab=expression(italic(A)[n]~~(mu*mol~m^-2~s^-1)),
                    xlim=c(0,1050))
    lines(Ci, Aj-Rd, col="blue", lwd=2)
    lines(Ci, ALEAF, lwd=2)
  })
  
  x <- plantecophys:::findCiTransition(Aci, Vcmax=65, Jmax=130, Rd=1.5)
  points(x, Aci(x, Vcmax=65, Jmax=130, Rd=1.5)$ALEAF, pch=21, cex=1.4, bg="white")
  
  legend("topleft", c(expression(A[j]),expression(A[c]), expression(A[n]), "Transition point"),
         lwd=c(2,2,2,-1), pch=c(-1,-1,-1,1), pt.cex=c(-1,-1,-1,1.4), col=c("blue","red","black", "black"),
         bty='n', cex=0.9)
  
  ci <- seq(50, 800, length=101)
  plotlabel("(a)", "bottomright")
  
  # PAnel b
  acirun <- Aci(Ci=ci)
  
  with(acirun, plot(Ci, ALEAF, type='l',
                    ylim=c(0,20), xlim=c(0,800),
                    lwd=2,
                    xlab=expression(italic(C)[i]~~(ppm)),
                    ylab=expression(italic(A)[n]~~(mu*mol~m^-2~s^-1))))
  
  p <- Photosyn(VPD=2.5, g1=2)
  
  gc <- p$GS / 1.57
  abline(gc*p$Ca, -gc,  lwd=2, lty=5)
  
  points(p$Ci, p$ALEAF, pch=22, bg="white", cex=1.4)
  plotlabel("(b)", "bottomright")
  
  legend("right", c("Demand","Supply","Solution"),
         lwd=c(2,2,-1),pch=c(-1,-1,0), pt.cex=c(-1,-1,1.4),
         inset=0.01, cex=0.8, bty='n', lty=c(1,5,-1))
}






# example output fitaci
figure2 <- function(){
  
  acidata1$PPFD <- 1800
  f <- suppressWarnings(fitaci(acidata1))
  
  par(mar=c(4,4,1,1), cex.lab=1.3, cex.axis=1, xaxs="i", yaxs="i",
      mgp=c(2,0.5,0), las=1, tcl=0.2)
  plot(f, xlim=c(0,1500), ylim=c(0,30), legendbty='n')
  
}



figure3 <- function(){
  
  #T response 
  vpdfun <- function(tair)0.000605*tair^2.39
  
  addone <- function(tair, vpd, linecol="black", ...){
    
    cis <- seq(50, 500, length=101)
    r1 <- Aci(cis, Tleaf=tair)
    with(r1, lines(Ci, ALEAF, type='l', col=linecol, lwd=2))
    
    r2 <- Photosyn(Ca=400, VPD=vpd, Tleaf=tair)
    #   gc <- r2$GS / 1.57
    #   ablinepiece(gc*r2$Ca, -gc,  col="darkgrey",
    #               from=r2$Ci, to=r2$Ca)
    points(r2$Ci, r2$ALEAF, pch=21, ...)
    
  }
  tairs <- seq(5,40, by=5)
  Cols <- heat.colors(length(tairs)+4)
  p <- Photosyn(Tleaf=tairs, VPD=vpdfun(tairs))
  
  
  par(mfrow=c(1,2), mar=c(4,4,1,1), xaxs="i", yaxs="i", cex.lab=1.2,
      mgp=c(2,0.5,0), las=1, tcl=0.2)
  plot(1, xlim=c(0,500), ylim=c(0,20), type='n',
       xlab=expression(italic(C)[i]~~(ppm)),
       ylab=expression(italic(A)[n]~~(mu*mol~m^-2~s^-1)))
  for(i in 1:length(tairs))addone(tairs[i], vpdfun(tairs[i]), linecol=Cols[i],
                                  cex=1.2,  bg=Cols[i])
  box()
  plotlabel("(a)","topleft")
#   for(i in 1:(nrow(p)-1)){
#     arrows(x0=p$Ci[i], x1=p$Ci[i+1], y0=p$ALEAF[i], y1=p$ALEAF[i+1],
#            col=Cols[i], length=0.15)
#   }
  legend("left", c(expression(T[leaf] == 5),
                      expression(T[leaf] == 40)),
         lty=1, lwd=2, col=Cols[c(1,length(tairs))], bty='n', cex=0.8)
  
  
  with(p, plot(Tleaf, ALEAF,
               ylim=c(0,20), xlim=c(0,45),
               xlab=expression(T[leaf]~~(degree*C)),
               ylab=expression(italic(A)[n]~~(mu*mol~m^-2~s^-1)),
               bg=Cols, pch=21, cex=1.2
  ))
  plotlabel("(b)","topleft")
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
  
  par(mfrow=c(1,2), mar=c(4,4,1,1), cex.lab=1.1,
      mgp=c(2,0.5,0), las=1, tcl=0.2)
  plot(1, type='n',        
       ylim=c(-1,10),
       xlim=c(0,400),
       xlab=expression(italic(C)[i]~~(ppm)),
       ylab=expression(A[n]-lambda*E~~(mu*mol~m^-2~s^-1)))
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
  


figure5 <- function(){
  
  # (a)
  par(mfrow=c(2,2), mar=c(4,4,1,1), cex=1.1, xaxs="i", yaxs="i",las=1,
      tcl=0.2,mgp=c(2,0.5,0),
      cex.axis=0.8)
  plot(acifits, "oneplot", addlegend=FALSE, highlight="25", transitionpoint=FALSE,
       ylim=c(-2,40), xlim=c(0,1100))
  box()
  plotlabel("(a)", "topleft")
  
  with(coef(acifits), plot(Vcmax, Jmax,
                           xlab=expression(V[cmax]~~(mu*mol~m^-2~s^-1)),
                           ylab=expression(J[max]~~(mu*mol~m^-2~s^-1)),
                     pch=19, 
                     xlim=c(40,160),
                     ylim=c(100,300)))
  predline(lm(Jmax ~ Vcmax, data=coef(acifits)))
  plotlabel("(b)", "topleft")
  
  with(tumspot_1, plot(GSpred, Cond, xlim=c(0,0.6), ylim=c(0,0.6),
                     xlab=expression(Modelled~g[s]~~(mol~m^-2~s^-1)),
                     ylab=expression(Measured~g[s]~~(mol~m^-2~s^-1)),
                     pch=19, col=alpha("black",0.3)))
  with(tumspot_2, points(GSpred, Cond, pch=19, col=alpha("red",0.3)))
  abline(0,1)
  plotlabel("(c)", "topleft")
  
  with(subset(tumspot_1,PARi > 1000), {
        plot(VpdL, Photo/Trmmol,
                    xlab="D (kPa)",
                    xlim=c(0,3), ylim=c(0,14),
                    ylab=expression(ITE~~(mu*mol~CO[2]/mmol~H[2]*O)),
                    pch=19, col=alpha("black",0.3))
        curve(0.1*mean(CO2S)/(1.6*(g1_1*sqrt(x) + x)), add=TRUE,
          from=min(VpdL), to=max(VpdL))
       })
  with(subset(tumspot_2,PARi > 1000), {
    points(VpdL, Photo/Trmmol,
         pch=19, col=alpha("red",0.3))
    curve(0.1*mean(CO2S)/(1.6*(g1_2*sqrt(x) + x)), add=TRUE,
          from=min(VpdL), to=max(VpdL))
  })
  
  plotlabel("(d)", "topleft")
  
}


