

# Read data for Figure 5.
# See : http://figshare.com/articles/Tumbarumba_Gas_Exchange/1538079

# See load.R for code to download the data to the cache/ subdirectory.

# A-Ci curve data
tum <- read.csv(acifn)
tumh <- subset(tum, PARi > 1400)

# Spot gas exchange data
tumspot <- read.csv(spotfn)


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
gfit <- fitBB(tumspot, gsmodel="BBOpti")
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

