
# See : http://figshare.com/articles/Tumbarumba_Gas_Exchange/1538079
process_acidata <- function(fn){

  # A-Ci curve data
  tum <- read.csv(acifn)
  
  # Use only light saturation (data include light response curves)
  tumh <- subset(tum, PARi > 1400)
  
  # Fit A-Ci curves for Figure 5.
  # poor curves (no saturation wrt Ci)
  bad <- c("12","18","39")
  tumh <- subset(tumh, !Curve %in% bad)
  
return(tumh)
}

process_spotdata <- function(fn){
  # Spot gas exchange data
  tumspot <- read.csv(spotfn)
  
  tumspot$Date <- as.Date(tumspot$Date, format="%d/%m/%Y")
  tumspot$Season <- factor(format(tumspot$Date, "%m-%Y"))
  
  # Choose only one season (the one with the best fit, of course)
  # tumspot <- subset(tumspot, Season %in% c("11-2001","05-2002"))
  tumspot <- subset(tumspot, Season %in% c("11-2001","05-2002"))

return(tumspot)
}

fit_medlynacicurves <- function(tumh){
  
  acifits <- fitacis(tumh, "Curve")
return(acifits)
}

fit_medlynBB <- function(tumspot){

  # Fit BB model to spot gas exchange data for Figure 5.
  gfit <- fitBB(tumspot, gsmodel="BBOpti")

return(gfit)
}

