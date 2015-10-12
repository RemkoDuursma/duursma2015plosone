
#-- Additional statistics

# The following bits of code show additional statistics that can be
# extracted from fitted A-Ci curves.
# Obviously, this list is not exhaustive.

# I use this example curve fit:
acidata1$PPFD <- 1800
myfit <- fitaci(acidata1)

# Show the usual output, including parameter estimates and confidence intervals.
summary(myfit)

# Correlation matrix of estimated Jmax, Vcmax and Rd from fitted A-Ci curve.
# Example given for one curve fitted above.
getCorMat_aci <- function(fit){
  x <- summary(myfit$nlsfit, correlation=TRUE)
  x$correlation
}
getCorMat_aci(myfit)


# R2 of observed vs. fitted.
getr2_acifit <- function(fit){
  x <- lm(Ameas ~ Amodel, data=fit$df)
  summary(x)$r.squared
}

getr2_acifit(myfit)

# Residual plot (residual vs. fitted)
residplot_acifit <- function(fit, ...){
  
  x <- myfit$df$Amodel
  y <- with(myfit$df, Ameas - Amodel)
  yl <- max(abs(y))
  plot(x, y, ylim=c(-1.2*yl, 1.2*yl),
       xlab="Fitted", ylab="Residual", ...)
  abline(h=0)
}
residplot_acifit(myfit)








