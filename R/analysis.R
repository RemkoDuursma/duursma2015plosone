
# Download data needed for Figure 5.
tum <- read.csv("TumbarumbaGasex_ACis_Medlyn.csv")
tumh <- subset(tum, PARi > 1400)

# lin <- read.csv("http://files.figshare.com/1886204/WUEdatabase_merged_Lin_et_al_2015_NCC.csv")
# tumspot <- subset(lin, Datacontrib == "Belinda Medlyn")
# saveRDS(tumspot, "tumspot.rds")
tumspot <- readRDS("tumspot.rds")


# Fit A-Ci curves for Figure 5.
# poor curves (no saturation)
bad <- c("12","18","39")

cf <- "cache/acifits.rds"
if(!file.exists(cf)){
  acifits <- fitacis(subset(tumh, !Curve %in% bad), "Curve")
  saveRDS(acifits, cf)
} else {
  acifits <- readRDS(cf)
}

# Fit BB model to spot gas exchange data for Figure 5.
gfit <- fitBB(tumspot, gsmodel="BBOpti",varnames=list(ALEAF="Photo",GS="Cond",VPD="VPD",
                                                          Ca="CO2S"))
tumspot$GSpred <- predict(gfit$fit, tumspot)
g1 <- coef(gfit)[[2]]
g1ci <- suppressMessages(unname(confint(gfit$fit)))

lmgpred <- lm(Cond ~ GSpred, data=tumspot)
lmgpredt <- tidy(lmgpred)
lmgpredg <- glance(lmgpred)


# Regression of Jmax vs. Vcmax (used in caption for Figure 5).
p <- coef(acifits)
lmjv <- lm(Jmax ~ Vcmax, data=p)
lmjvt <- tidy(lmjv)
lmjvg <- glance(lmjv)
