

# Read data for Figure 5.
# See : http://figshare.com/articles/Tumbarumba_Gas_Exchange/1538079

# See load.R for code to download the data to the cache/ subdirectory.

# A-Ci curve data
tum <- read.csv(acifn)
tumh <- subset(tum, PARi > 1400)

# Spot gas exchange data
tumspot <- read.csv(spotfn)

tumspot$Date <- as.Date(tumspot$Date, format="%d/%m/%Y")
tumspot$Season <- factor(format(tumspot$Date, "%m-%Y"))

# Choose only one season (the one with the best fit, of course)
# tumspot <- subset(tumspot, Season %in% c("11-2001","05-2002"))
tumspot_1 <- subset(tumspot, Season == "11-2001")
tumspot_2 <- subset(tumspot, Season == "05-2002")

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
gfit_1 <- fitBB(tumspot_1, gsmodel="BBOpti")
gfit_2 <- fitBB(tumspot_2, gsmodel="BBOpti")

tumspot_1$GSpred <- predict(gfit_1$fit, tumspot_1)
tumspot_2$GSpred <- predict(gfit_2$fit, tumspot_2)

g1_1 <- coef(gfit_1)[[2]]
g1ci_1 <- suppressMessages(unname(confint(gfit_1$fit)))

g1_2 <- coef(gfit_2)[[2]]
g1ci_2 <- suppressMessages(unname(confint(gfit_2$fit)))


lmgpred_1 <- lm(Cond ~ GSpred, data=tumspot_1)
lmgpred_2 <- lm(Cond ~ GSpred, data=tumspot_2)
lmgpredt_1 <- tidy(lmgpred_1)
lmgpredt_2 <- tidy(lmgpred_2)
lmgpredg_1 <- glance(lmgpred_1)
lmgpredg_2 <- glance(lmgpred_2)

# Regression of Jmax vs. Vcmax (used in caption for Figure 5).
p <- coef(acifits)
lmjv <- lm(Jmax ~ Vcmax, data=p)
lmjvt <- tidy(lmjv)
lmjvg <- glance(lmjv)

