# Figures
source("R/functions.R")
library(plantecophys)

figure1 <- function(){
  
  
  f <- fitaci(acidata1)
  
  par(mar=c(5,5,1,1), cex.lab=1.3, cex.axis=1)
  plot(f)
  
}
to.pdf(figure1(), filename="manuscript/figures/figure1.pdf")
