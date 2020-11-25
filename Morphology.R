setwd("..")

#upload datasets - all data should be available on the BNZ LTER data portal
# VACULI is all V. uliginosum data (blueberry)
# VACVIT is all V. vitis-idaea data (low bush cranberry / lingonberry)
# BOTH has all data

VACULI <- read.csv("VACULI.csv")
VACVIT <- read.csv("VACVIT.csv")
BOTH <- read.csv("BOTH.csv")

#seperate dataframe for upland and lowland calculations
VACULI_PC1Pos <- subset(VACULI, VACULI$PC1>0)
VACULI_PC1Neg <- subset(VACULI, VACULI$PC1 <0)
VACVIT_PC1Pos <- subset(VACVIT, VACVIT$PC1>0)
VACVIT_PC1Neg <- subset(VACVIT, VACVIT$PC1<0)
BOTH_PC1Pos <- subset(BOTH, BOTH$PC1>0)
BOTH_PC1Neg <- subset(BOTH, BOTH$PC1<0)

#creating a Upland (High) and Lowland (Low) variable in the full datasets
VACVIT$YN.pc1<- ifelse(VACVIT$PC1>0, 'High', 'Low')
VACULI$YN.pc1 <- ifelse(VACULI$PC1>0, 'High', 'Low')
BOTH$YN.pc1 <- ifelse(BOTH$PC1>0, 'High', 'Low')

#What follows is an incredibly messy shitload of calculations and graphs to explore 
# the relationships between the berry plants and the environment 
# as well as the biomass ratios within the plants 

# CoV is the coefficient of variation


#Canopy cover
mean(c(VACVIT_PC1Pos$pct.canopy, VACULI_PC1Pos$pct.canopy), na.rm=TRUE)
sd(c(VACVIT_PC1Pos$pct.canopy, VACULI_PC1Pos$pct.canopy), na.rm=TRUE)
mean(c(VACVIT_PC1Neg$pct.canopy, VACULI_PC1Neg$pct.canopy), na.rm=TRUE)
sd(c(VACVIT_PC1Neg$pct.canopy, VACULI_PC1Neg$pct.canopy), na.rm=TRUE)

anova(lm(VACULI$pct.canopy~VACULI$YN.pc1))
anova(lm(BOTH$pct.canopy~BOTH$YN.pc1))

#CoV flowers within site - VACVIT
VVi.FlwrMean.site <- aggregate(VACVIT$X.Flwrs, by = list(VACVIT$Site), mean, na.rm=TRUE)
VVi.FlwrMean.site

Vvi.FlwrSD.site <- aggregate(VACVIT$X.Flwrs, by = list(VACVIT$Site), sd, na.rm=TRUE)
Vvi.FlwrSD.site

Vvi.FlwrCov.site <- Vvi.FlwrSD.site$x/VVi.FlwrMean.site$x
Vvi.FlwrCov.site

#CoV berries within site - VACVIT
VVi.BryMean.site <- aggregate(VACVIT$X.Berries, by = list(VACVIT$Site), mean, na.rm=TRUE)
VVi.BryMean.site

Vvi.BrySD.site <- aggregate(VACVIT$X.Berries, by = list(VACVIT$Site), sd, na.rm=TRUE)
Vvi.BrySD.site

Vvi.BryCov.site <- Vvi.BrySD.site$x/VVi.BryMean.site$x
Vvi.BryCov.site


#CoV flowers within site - VACULI
Vu.FlwrMean.site <- aggregate(VACULI$X.Flwrs, by = list(VACULI$Site), mean, na.rm=TRUE)
Vu.FlwrMean.site

Vu.FlwrSD.site <- aggregate(VACULI$X.Flwrs, by = list(VACULI$Site), sd, na.rm=TRUE)
Vu.FlwrSD.site

Vu.FlwrCov.site <- Vu.FlwrSD.site$x/Vu.FlwrMean.site$x
Vu.FlwrCov.site

#CoV berries within site - VACULI
Vu.BryMean.site <- aggregate(VACULI$X.Berries, by = list(VACULI$Site), mean, na.rm=TRUE)
Vu.BryMean.site

Vu.BrySD.site <- aggregate(VACULI$X.Berries, by = list(VACULI$Site), sd, na.rm=TRUE)
Vu.BrySD.site

Vu.BryCov.site <- Vu.BrySD.site$x/Vu.BryMean.site$x
Vu.BryCov.site

#--------------------------------------------------------------------------#
#CoV flowers across sites - VACVIT  
Vvi.FlwrMean.across <- mean(VVi.FlwrMean.site$x)
Vvi.FlwrSD.across <- sd(VVi.FlwrMean.site$x)
Vvi.FlwrCov.across <- Vvi.FlwrSD.across/Vvi.FlwrMean.across
Vvi.FlwrCov.across

#CoV flowers across sites - VACULI
Vu.FlwrMean.across <- mean(Vu.FlwrMean.site$x)
Vu.FlwrSD.across <- sd(Vu.FlwrMean.site$x)
Vu.FlwrCov.across <- Vu.FlwrSD.across/Vu.FlwrMean.across
Vu.FlwrCov.across

#CoV berries across sites - VACVIT  
Vvi.BryMean.across <- mean(VVi.BryMean.site$x)
Vvi.BrySD.across <- sd(VVi.BryMean.site$x)
Vvi.BryCov.across <- Vvi.BrySD.across/Vvi.BryMean.across
Vvi.BryCov.across

#CoV berries across sites - VACULI
Vu.BryMean.across <- mean(Vu.BryMean.site$x)
Vu.BrySD.across <- sd(Vu.BryMean.site$x)
Vu.BryCov.across <- Vu.BrySD.across/Vu.BryMean.across
Vu.BryCov.across

#----------------------------------------------------------------------------
# Leaf:stem ratio ~ canopy cover - VACVIT
VACVIT$LPR <- VACVIT$Leaf..g./VACVIT$Stem..g.

plot(VACVIT$LPR ~ VACVIT$pct.canopy)
abline(lm(VACVIT$LPR ~ VACVIT$pct.canopy))
summary(lm(log(VACVIT$LPR+1) ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$LPR ~ VACVIT$pct.canopy))

summary(lm(VACVIT$LPR ~ VACVIT$pct.canopy))

summary(aov(VACVIT$LPR ~ VACVIT$pct.canopy*VACVIT$YNcanopy))
LPR.vi1<- aov(VACVIT$LPR ~ VACVIT$pct.canopy+VACVIT$YNcanopy)
LPR.vi2<- aov(VACVIT$LPR ~ VACVIT$pct.canopy*VACVIT$YNcanopy)
anova(LPR.vi1, LPR.vi2)

VACVIT$LPR.std <- (VACVIT$LPR - mean(VACVIT$LPR, na.rm=T))/ sd(VACVIT$LPR, na.rm=T)


#Leaf:stem ratio ~ canopy cover - VACULI
VACULI$LPR <- as.numeric(as.character(VACULI$Leaf..g.))/as.numeric(as.character(VACULI$Stem..g.))

plot(VACULI$LPR ~ VACULI$pct.canopy)
abline(lm(VACULI$LPR ~ VACULI$pct.canopy))
summary(lm(log(VACULI$LPR+1) ~ VACULI$pct.canopy))
AIC(lm(VACULI$LPR ~ VACULI$pct.canopy))
summary(lm(VACULI$LPR ~ VACULI$pct.canopy))


summary(aov(VACULI$LPR ~ VACULI$pct.canopy*VACULI$YNcanopy))
LPR.1 <- aov(VACULI$LPR ~ VACULI$pct.canopy+VACULI$YNcanopy)
LPR.2 <- aov(VACULI$LPR ~ VACULI$pct.canopy*VACULI$YNcanopy)
anova(LPR.1, LPR.2)

plot(resid(LPR.1))
plot(LPR.1)

VACULI$LPR.std <- (VACULI$LPR - mean(VACULI$LPR, na.rm=T))/ sd(VACULI$LPR, na.rm=T)
VACULI$canopy.std <- (VACULI$pct.canopy - mean(VACULI$pct.canopy, na.rm=T))/ sd(VACULI$pct.canopy, na.rm=T)

#-----------------------------------------------------------------------------
# nFlowers:stem mass ~ canopy cover - VACVIT
VACVIT$FSR <- VACVIT$X.Flwrs/VACVIT$Stem..g.

plot(VACVIT$FSR ~ VACVIT$pct.canopy)
abline(lm(VACVIT$FSR ~ VACVIT$pct.canopy))
summary(lm(log(VACVIT$FSR+1) ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$FSR ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$FSR ~ VACVIT$pct.canopy + I(VACVIT$pct.canopy^2)))
AIC(lm(log(VACVIT$FSR+1) ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$FSR ~ 1/VACVIT$pct.canopy))
AIC(lm(VACVIT$FSR ~ log(VACVIT$pct.canopy)))
AIC(lm(log(VACVIT$FSR+1) ~ 1/VACVIT$pct.canopy))
AIC(lm(log(VACVIT$FSR+1) ~ VACVIT$pct.canopy + I (VACVIT$pct.canopy^2)))
AIC(lm(log(VACVIT$FSR+1) ~ log(VACVIT$pct.canopy)))

summary(aov(log(VACVIT$FSR+1) ~ VACVIT$pct.canopy+VACVIT$PC1))


# nFlowers:stem mass ~ canopy cover - VACULI
VACULI$FSR <- VACULI$X.Flwrs/VACULI$Stem..g.

plot(VACULI$FSR ~ VACULI$pct.canopy)
abline(lm(VACULI$FSR ~ VACULI$pct.canopy))
summary(lm(log(VACULI$FSR+1) ~ VACULI$pct.canopy))
AIC(lm(VACULI$FSR ~ VACULI$pct.canopy))
AIC(lm(VACULI$FSR ~ VACULI$pct.canopy + I(VACULI$pct.canopy^2)))
AIC(lm(log(VACULI$FSR+1) ~ VACULI$pct.canopy))
AIC(lm(VACULI$FSR ~ 1/VACULI$pct.canopy))
AIC(lm(VACULI$FSR ~ log(VACULI$pct.canopy)))
AIC(lm(log(VACULI$FSR+1) ~ 1/VACULI$pct.canopy))
AIC(lm(log(VACULI$FSR+1) ~ VACULI$pct.canopy + I(VACULI$pct.canopy^2)))
AIC(lm(log(VACULI$FSR+1) ~ log(VACULI$pct.canopy)))

#nBerries : stem mass ~ canopy
VACVIT$BSR <- VACVIT$X.Berries/VACVIT$Stem..g.
VACULI$BSR <- VACULI$X.Berries/VACULI$Stem..g.

summary(lm(log(VACVIT$BSR+1)~VACVIT$pct.canopy))
summary(lm(log(VACULI$BSR+1)~VACULI$pct.canopy))

#------------------------------------------------------------------------------
#nFlowers: leaf mass ~ canopy cover - VACVIT
VACVIT$FLR <- VACVIT$X.Flwrs/VACVIT$Leaf..g.

plot(VACVIT$FLR ~ VACVIT$pct.canopy)
abline(lm(VACVIT$FLR ~ VACVIT$pct.canopy))
summary(lm(log(VACVIT$FLR+1) ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$FLR ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$FLR ~ VACVIT$pct.canopy + I(VACVIT$pct.canopy^2)))
AIC(lm(log(VACVIT$FLR+1) ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$FLR ~ 1/VACVIT$pct.canopy))
AIC(lm(VACVIT$FLR ~ log(VACVIT$pct.canopy)))
summary(lm(log(VACVIT$FLR+1) ~ VACVIT$pct.canopy))
plot()

summary(lm(VACVIT$FLR ~ VACVIT$pct.canopy))

#nFlowers: leaf mass ~ canopy cover - VACULI
VACULI$FLR <- VACULI$X.Flwrs/as.numeric(as.character(VACULI$Leaf..g.))
AIC(lm(VACULI$FLR ~ VACULI$pct.canopy))
AIC(lm(VACULI$FLR ~ VACULI$pct.canopy + I(VACULI$pct.canopy^2)))
AIC(lm(VACULI$FLR ~ 1/VACULI$pct.canopy))
AIC(lm(VACULI$FLR ~ log(VACULI$pct.canopy)))
AIC(lm(log(VACULI$FLR+1) ~ VACULI$pct.canopy))
AIC(lm(log(VACULI$FLR+1) ~ 1/VACULI$pct.canopy))
AIC(lm(log(VACULI$FLR+1) ~ VACULI$pct.canopy + I(VACULI$pct.canopy^2)))
AIC(lm(log(VACULI$FLR+1) ~ log(VACULI$pct.canopy)))

summary(lm(VACULI$FLR ~ VACULI$pct.canopy))


#nBerries : Leaf mass ~ canopy
VACULI$BLR <- VACULI$X.Berries/as.numeric(as.character(VACULI$Leaf..g.))
VACVIT$BLR <- VACVIT$X.Berries/(VACVIT$Leaf..g.)

summary(lm(log(VACVIT$BLR+1) ~ VACVIT$pct.canopy))
summary(lm(log(VACULI$BLR+1) ~ VACULI$pct.canopy))

summary(lm(log(VACULI$BLR+1) ~ VACULI$PC1))

summary(aov(log(VACVIT$BLR+1) ~ VACVIT$pct.canopy*VACVIT$YN.pc1))
summary(aov(log(VACULI$BLR+1) ~ VACULI$pct.canopy*VACULI$YN.pc1))

#------------------------------------------------------------------------
#nFlowers : plant mass ~ canopy cover - VACVIT
VACVIT$FPR <- VACVIT$X.Flwrs/(VACVIT$Leaf..g. + VACVIT$Stem..g.)

summary(lm(log(VACVIT$FPR+1) ~ VACVIT$pct.canopy))

AIC(lm(VACVIT$FPR ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$FPR ~ VACVIT$pct.canopy + I(VACVIT$pct.canopy^2)))
AIC(lm(VACVIT$FPR ~ 1/VACVIT$pct.canopy))
AIC(lm(VACVIT$FPR ~ log(VACVIT$pct.canopy)))
AIC(lm(log(VACVIT$FPR+1) ~ VACVIT$pct.canopy))
AIC(lm(log(VACVIT$FPR+1) ~ 1/VACVIT$pct.canopy))
AIC(lm(log(VACVIT$FPR+1) ~ VACVIT$pct.canopy + I(VACVIT$pct.canopy^2)))
AIC(lm(log(VACVIT$FPR+1) ~ log(VACVIT$pct.canopy)))

VACVIT$YNcanopy <- ifelse(VACVIT$pct.canopy > 50, "High", "Low")

summary(aov(VACVIT$FPR ~ VACVIT$pct.canopy*VACVIT$YNcanopy))
# there is no difference in flower:plant ratio between high and low canopy VACVIT

VACVIT$FPR.std <- (VACVIT$FPR - mean(VACVIT$FPR, na.rm=T))/ sd(VACVIT$FPR, na.rm=T)
VACVIT$canopy.std <- (VACVIT$pct.canopy - mean(VACVIT$pct.canopy, na.rm=T))/ sd(VACVIT$pct.canopy, na.rm=T)
summary(lm(VACVIT$FPR.std~VACVIT$canopy.std))


#nFlowers : plant mass ~ canopy cover - VACULI
VACULI$FPR <- VACULI$X.Flwrs/(as.numeric(as.character(VACULI$Leaf..g.)) + VACULI$Stem..g.)

summary(lm(log(VACULI$FPR+1) ~ VACULI$pct.canopy))
AIC(lm(VACULI$FPR ~ VACULI$pct.canopy))
AIC(lm(VACULI$FPR ~ VACULI$pct.canopy + I(VACULI$pct.canopy^2)))
AIC(lm(VACULI$FPR ~ 1/VACULI$pct.canopy))
AIC(lm(VACULI$FPR ~ log(VACULI$pct.canopy)))
AIC(lm(log(VACULI$FPR+1) ~ VACULI$pct.canopy))
AIC(lm(log(VACULI$FPR+1) ~ 1/VACULI$pct.canopy))
AIC(lm(log(VACULI$FPR+1) ~ VACULI$pct.canopy + I(VACULI$pct.canopy^2)))
AIC(lm(log(VACULI$FPR+1) ~ log(VACULI$pct.canopy)))

plot(VACULI$FPR ~ VACULI$pct.canopy)
Vu.highcanopy <- subset(VACULI, pct.canopy > 50)
Vu.lowcanopy <- subset(VACULI, pct.canopy < 51)

VACULI$YNcanopy <- ifelse(VACULI$pct.canopy > 50, "High", "Low")

summary(aov(VACULI$FPR ~ VACULI$pct.canopy*VACULI$YNcanopy))
# there is no difference in flower:plant ratio between high and low canopy VACULI


VACULI$FPR.std <- (VACULI$FPR - mean(VACULI$FPR, na.rm=T))/ sd(VACULI$FPR, na.rm=T)
VACULI$canopy.std <- (VACULI$pct.canopy - mean(VACULI$pct.canopy, na.rm=T))/ sd(VACULI$pct.canopy, na.rm=T)
summary(lm(VACULI$FPR.std~VACULI$canopy.std))
plot(VACULI$FPR.std~VACULI$canopy.std)


#---------------------------------------------------------------------------
#nFlowers : nLeaves ~ canopy - VACVIT
VACVIT$NFL <- VACVIT$X.Flwrs/VACVIT$X.Leaves

summary(lm(log(VACVIT$NFL+1) ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$NFL ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$NFL ~ VACVIT$pct.canopy + I(VACVIT$pct.canopy^2)))
AIC(lm(VACVIT$NFL ~ 1/VACVIT$pct.canopy))
AIC(lm(VACVIT$NFL ~ log(VACVIT$pct.canopy)))
AIC(lm(log(VACVIT$NFL+1) ~ VACVIT$pct.canopy))
AIC(lm(log(VACVIT$NFL+1) ~ 1/VACVIT$pct.canopy))
AIC(lm(log(VACVIT$NFL+1) ~ VACVIT$pct.canopy + I(VACVIT$pct.canopy^2)))
AIC(lm(log(VACVIT$NFL+1) ~ log(VACVIT$pct.canopy)))

VACVIT$NFL.std <- (VACVIT$NFL - mean(VACVIT$NFL, na.rm=T))/ sd(VACVIT$NFL, na.rm=T)
summary(aov(VACVIT$NFL ~ VACVIT$canopy*VACVIT$YNcanopy))


#nFlowers : nLeaves ~ canopy - VACULI
VACULI$NFL <- VACULI$X.Flwrs/VACULI$X.Leaves

summary(lm(log(VACULI$NFL+1) ~ VACULI$pct.canopy))
AIC(lm(VACULI$NFL ~ VACULI$pct.canopy))
AIC(lm(VACULI$NFL ~ VACULI$pct.canopy + I(VACULI$pct.canopy^2)))
AIC(lm(VACULI$NFL ~ 1/VACULI$pct.canopy))
AIC(lm(VACULI$NFL ~ log(VACULI$pct.canopy)))
AIC(lm(log(VACULI$NFL+1) ~ VACULI$pct.canopy))
AIC(lm(log(VACULI$NFL+1) ~ 1/VACULI$pct.canopy))
AIC(lm(log(VACULI$NFL+1) ~ VACULI$pct.canopy + I(VACULI$pct.canopy^2)))
AIC(lm(log(VACULI$NFL+1) ~ log(VACULI$pct.canopy)))

summary(aov(log(VACULI$NFL+1) ~ VACULI$pct.canopy*VACULI$YNcanopy))

summary(aov(VACULI$NFL.std ~ VACULI$canopy.std*VACULI$YNcanopy))
summary(aov(log(VACULI$NFL.std) ~ log(VACULI$canopy.std+1)*VACULI$YNcanopy))


VACULI$NFL.std <- (VACULI$NFL - mean(VACULI$NFL, na.rm=T))/ sd(VACULI$NFL, na.rm=T)

#--------------------------------------------------------------------------
#nLeaves : Plant mass ~ canopy cover - VACVIT
VACVIT$NLP <- VACVIT$X.Leaves/(VACVIT$Stem..g.+VACVIT$Leaf..g.) 

summary(lm(log(VACVIT$NLP+1) ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$NLP ~ VACVIT$pct.canopy))
AIC(lm(VACVIT$NLP ~ VACVIT$pct.canopy + I(VACVIT$pct.canopy^2)))
AIC(lm(VACVIT$NLP ~ 1/VACVIT$pct.canopy))
AIC(lm(VACVIT$NLP ~ log(VACVIT$pct.canopy)))
AIC(lm(log(VACVIT$NLP+1) ~ VACVIT$pct.canopy))
AIC(lm(log(VACVIT$NLP+1) ~ 1/VACVIT$pct.canopy))
AIC(lm(log(VACVIT$NLP+1) ~ VACVIT$pct.canopy + I(VACVIT$pct.canopy^2)))
AIC(lm(log(VACVIT$NLP+1) ~ log(VACVIT$pct.canopy)))

#nLeaves : Plant mass ~ canopy cover - VACULI
VACULI$NLP <- VACULI$X.Leaves/(VACULI$Stem..g.+as.numeric(as.character(VACULI$Leaf..g.))) 

summary(lm(log(VACULI$NLP+1) ~ VACULI$pct.canopy))
AIC(lm(VACULI$NLP ~ VACULI$pct.canopy))
AIC(lm(VACULI$NLP ~ VACULI$pct.canopy + I(VACULI$pct.canopy^2)))
AIC(lm(VACULI$NLP ~ 1/VACULI$pct.canopy))
AIC(lm(VACULI$NLP ~ log(VACULI$pct.canopy)))
AIC(lm(log(VACULI$NLP+1) ~ VACULI$pct.canopy))
AIC(lm(log(VACULI$NLP+1) ~ 1/VACULI$pct.canopy))
AIC(lm(log(VACULI$NLP+1) ~ VACULI$pct.canopy + I(VACULI$pct.canopy^2)))
AIC(lm(log(VACULI$NLP+1) ~ log(VACULI$pct.canopy)))

#nBerries : nLeaves ~ canopy cover - VACVIT
VACVIT$NBL <- VACVIT$X.Berries/VACVIT$X.Leaves
VACULI$NBL <- VACULI$X.Berries/VACULI$X.Leaves

summary(lm(log(VACVIT$NBL+1) ~ VACVIT$pct.canopy))
summary(lm(log(VACULI$NBL+1) ~ VACULI$pct.canopy))

#nBerries : Plant mass ~ canopy
VACVIT$NBP <- VACVIT$X.Berries/(VACVIT$Leaf..g.+VACVIT$Stem..g.)
VACULI$NBP <- VACULI$X.Berries/(as.numeric(as.character(VACULI$Leaf..g.))+VACULI$Stem..g.)

summary(lm(log(VACVIT$NBP+1) ~ VACVIT$pct.canopy))
summary(lm(log(VACULI$NBP+1) ~ VACULI$pct.canopy))

#-------------------------------------------------------
#Berry mass : Leaf mass ~ canopy
VACVIT$BLM <- VACVIT$Berry..g./(VACVIT$Leaf..g.)
VACULI$BLM <- VACULI$Berry..g./as.numeric(as.character(VACULI$Leaf..g.))

summary(lm(VACVIT$BLM ~ VACVIT$pct.canopy))
summary(lm(VACULI$BLM ~ VACULI$pct.canopy))

#---------------------------------------------------------------------------------------------

BOTH$FPR <- BOTH$X.Flwrs/(as.numeric(as.character(BOTH$Leaf..g.)) + BOTH$Stem..g.)

BOTH$YNcanopy <- ifelse(BOTH$pct.canopy > 50, "High", "Low")

BOTH$FPR.std <- (BOTH$FPR - mean(BOTH$FPR, na.rm=T))/ sd(BOTH$FPR, na.rm=T)
BOTH$canopy.std <- (BOTH$pct.canopy - mean(BOTH$pct.canopy, na.rm=T))/ sd(BOTH$pct.canopy, na.rm=T)
summary(lm(BOTH$FPR.std~BOTH$canopy.std*BOTH$Species))
plot(BOTH$FPR.std~BOTH$canopy.std*BOTH$Species, col= BOTH$Species)
abline(lm(BOTH$FPR.std~BOTH$canopy.std, Species == "VACVIT", data = BOTH))




FPR.1 <- lm(VACULI$FPR.std ~VACULI$canopy.std *VACULI$Species)
FPR.2 <- lm(VACVIT$FPR.std~VACVIT$canopy.std*VACVIT$Species)
anova(FPR.B1, FPR.B2)

plot(VACVIT$canopy.std, VACVIT$FPR.std, col='red', xlab = "canopy cover (standardized)", ylab = "nFlowers : plant mass (standardized)")
points(VACULI$canopy.std, VACULI$FPR.std, col='blue')
abline(lm(VACVIT$FPR.std~VACVIT$canopy.std))
abline(lm(VACULI$FPR.std ~ VACULI$canopy.std))


plot(VACVIT$canopy.std, VACVIT$LPR.std, col='red', xlab = "canopy cover (standardized)", ylab = "leaf mass : stem mass (standardized)")
points(VACULI$canopy.std, VACULI$LPR.std, col='blue')
abline(lm(VACVIT$LPR.std~VACVIT$canopy.std))
abline(lm(VACULI$LPR.std~VACULI$canopy.std))
summary(lm(VACVIT$LPR.std~VACVIT$canopy.std))
summary(lm(VACULI$LPR.std~VACULI$canopy.std))



plot(VACVIT$canopy.std, VACVIT$NFL.std, col= 'red', xlab = "canopy cover (standardized)", ylab = "nFlowers : nLeaves (standardized)") 
points(VACULI$canopy.std, VACULI$NFL.std, col='blue')
abline(lm(VACVIT$NFL.std~VACVIT$canopy.std))
abline(lm(VACULI$NFL.std~VACULI$canopy.std))

STNDRD <- read.csv("standards.csv")
std1 <-  aov(STNDRD$LPR.std~STNDRD$canopy.std*STNDRD$Species)
std2 <- aov(STNDRD$LPR.std~STNDRD$canopy.std+STNDRD$Species)

summary(aov(STNDRD$FPR.std ~STNDRD$canopy.std*STNDRD$Species))
summary(aov(STNDRD$LPR.std ~STNDRD$canopy.std*STNDRD$Species))
summary(aov(STNDRD$NFL.std ~ STNDRD$canopy.std*STNDRD$Species))


anova(std1, std2)
summary(std1)

#----------------------------------------------------------------------------
summary(lm(VACULI$LPR~VACULI$pct.canopy))
summary(lm(VACULI$FLR~VACULI$pct.canopy))
summary(lm(VACULI$BLM~VACULI$pct.canopy))

summary(lm(VACVIT$LPR~VACVIT$pct.canopy))
summary(lm(VACVIT$FLR~VACVIT$pct.canopy))
summary(lm(VACVIT$BLM~VACVIT$pct.canopy))

summary(lm(VACVIT_PC1Pos$LPR~VACVIT_PC1Pos$pct.canopy))
summary(lm(VACVIT_PC1Pos$FLR~VACVIT_PC1Pos$pct.canopy))
summary(lm(VACVIT_PC1Pos$BLM~VACVIT_PC1Pos$pct.canopy))

summary(lm(VACVIT_PC1Neg$LPR~VACVIT_PC1Neg$pct.canopy))
summary(lm(VACVIT_PC1Neg$FLR~VACVIT_PC1Neg$pct.canopy))
summary(lm(VACVIT_PC1Neg$BLM~VACVIT_PC1Neg$pct.canopy))

summary(lm(VACULI_PC1Pos$LPR~VACULI_PC1Pos$pct.canopy))
summary(lm(VACULI_PC1Pos$FLR~VACULI_PC1Pos$pct.canopy))
summary(lm(VACULI_PC1Pos$BLM~VACULI_PC1Pos$pct.canopy))

summary(lm(VACULI_PC1Neg$LPR~VACULI_PC1Neg$pct.canopy))
summary(lm(VACULI_PC1Neg$FLR~VACULI_PC1Neg$pct.canopy))
summary(lm(VACULI_PC1Neg$BLM~VACULI_PC1Neg$pct.canopy))
#-----------------------------------------------------------------------------
summary(lm(VACVIT_PC1Neg$NFL~VACVIT_PC1Neg$PC2))

a1 <- (VACVIT_PC1Pos$NBP - mean(VACVIT_PC1Pos$NBP, na.rm=T))/sd(VACVIT_PC1Pos$NBP, na.rm=T)
a2 <- (VACVIT_PC1Pos$pct.canopy - mean(VACVIT_PC1Pos$pct.canopy, na.rm=T))/sd(VACVIT_PC1Pos$pct.canopy, na.rm=T)
b1 <- (VACVIT_PC1Neg$NBP - mean(VACVIT_PC1Neg$NBP, na.rm=T))/sd(VACVIT_PC1Neg$NBP, na.rm=T)
b2 <- (VACVIT_PC1Neg$pct.canopy - mean(VACVIT_PC1Neg$pct.canopy, na.rm=T))/sd(VACVIT_PC1Neg$pct.canopy, na.rm=T)
c1 <- (VACULI_PC1Pos$NBP - mean(VACULI_PC1Pos$NBP, na.rm=T))/sd(VACULI_PC1Pos$NBP, na.rm=T)
c2 <- (VACULI_PC1Pos$pct.canopy - mean(VACULI_PC1Pos$pct.canopy, na.rm=T))/sd(VACULI_PC1Pos$pct.canopy, na.rm=T)
d1 <- (VACULI_PC1Neg$NBP - mean(VACULI_PC1Neg$NBP, na.rm=T))/sd(VACULI_PC1Neg$NBP, na.rm=T)
d2 <- (VACULI_PC1Neg$pct.canopy - mean(VACULI_PC1Neg$pct.canopy, na.rm=T))/sd(VACULI_PC1Neg$pct.canopy, na.rm=T)


plot(a2, a1, col= 'red', xlab = "canopy cover (standardized)", ylab = "nBerries : Plant Mass (standardized)") 
points(b2, b1, col='pink')
points(c2, c1, col='blue')
points(d2, d1, col='light blue')
abline(lm(VACVIT$NFL.std~VACVIT$canopy.std))
abline(lm(VACULI$NFL.std~VACULI$canopy.std))

#---------------------------------------------------------------------------------
summary(aov(log(BOTH$FPR+1)~BOTH$pct.canopy*BOTH$Species))

#-----------------------------------------------------------------------------------
t.test(Vvi.FlwrCov.site, Vu.FlwrCov.site, paired=T)
t.test(Vvi.BryCov.site, Vu.BryCov.site, paired=T)


VACVIT$LMF <- VACVIT$Leaf..g./(VACVIT$Leaf..g.+VACVIT$Berry..g.+VACVIT$Stem..g.)
VACULI$LMF <- as.numeric(as.character(VACULI$Leaf..g.))/(as.numeric(as.character(VACULI$Leaf..g.))+as.numeric(as.character(VACULI$Berry..g.))+VACULI$Stem..g.)

summary(lm(VACVIT$LMF~VACVIT$pct.canopy))
summary(lm(VACULI$LMF~VACULI$pct.canopy))

VACVIT$SMF <- VACVIT$Stem..g./(VACVIT$Leaf..g.+VACVIT$Berry..g.+VACVIT$Stem..g.)
VACULI$SMF <- VACULI$Stem..g./(as.numeric(as.character(VACULI$Leaf..g.))+VACULI$Berry..g.+VACULI$Stem..g.)

summary(lm(VACVIT$SMF~VACVIT$pct.canopy))
summary(lm(VACULI$SMF~VACULI$pct.canopy))

summary(lm(VACVIT_PC1Pos$SMF~VACVIT_PC1Pos$pct.canopy))
summary(lm(VACVIT_PC1Neg$SMF~VACVIT_PC1Neg$pct.canopy))
summary(lm(VACULI_PC1Pos$SMF~VACULI_PC1Pos$pct.canopy))
summary(lm(VACULI_PC1Neg$SMF~VACULI_PC1Neg$pct.canopy))
summary(lm(VACULI$SMF~VACULI$pct.canopy))
summary(lm(VACVIT$SMF~VACVIT$pct.canopy))

VACVIT$BMF <- VACVIT$Berry..g./(VACVIT$Leaf..g.+VACVIT$Berry..g.+VACVIT$Stem..g.)
VACULI$BMF <- VACULI$Berry..g./(as.numeric(as.character(VACULI$Leaf..g.))+VACULI$Berry..g.+VACULI$Stem..g.)

summary(lm(VACVIT$BMF~VACVIT$pct.canopy))
summary(lm(VACULI$BMF~VACULI$pct.canopy))

#------Upland and lowland -----

VACVIT_PC1Pos$BMF <- VACVIT_PC1Pos$Berry..g./(VACVIT_PC1Pos$Leaf..g.+VACVIT_PC1Pos$Berry..g.+VACVIT_PC1Pos$Stem..g.)
VACVIT_PC1Neg$BMF <- VACVIT_PC1Neg$Berry..g./(VACVIT_PC1Neg$Leaf..g.+VACVIT_PC1Neg$Berry..g.+VACVIT_PC1Neg$Stem..g.)

summary(lm(VACVIT_PC1Pos$BMF~VACVIT_PC1Pos$pct.canopy))
summary(lm(VACVIT_PC1Neg$BMF~VACVIT_PC1Neg$pct.canopy))


VACULI$BMF <- VACULI$Berry..g./(as.numeric(as.character(VACULI$Leaf..g.))+VACULI$Berry..g.+VACULI$Stem..g.)

#-------
  
anova(lm(VACULI_PC1Pos$pct.canopy ~VACULI_PC1Neg$pct.canopy), na.rm=T)

#-------

library(lme4)
library(lmerTest)

VACVIT$CanCovAvg <- ave(VACVIT$pct.canopy, VACVIT$Site, na.rm=T)
VACVIT_PC1Neg$CanCovAvg <- ave(VACVIT_PC1Neg$pct.canopy, VACVIT_PC1Neg$Site, na.rm=T)
VACVIT_PC1Pos$CanCovAvg <- ave(VACVIT_PC1Pos$pct.canopy, VACVIT_PC1Pos$Site, na.rm=T)

VACULI$CanCovAvg <- ave(VACULI$pct.canopy, VACULI$Site, na.rm=T)
VACULI_PC1Neg$CanCovAvg <- ave(VACULI_PC1Neg$pct.canopy, VACULI_PC1Neg$Site, na.rm=T)
VACULI_PC1Pos$CanCovAvg <- ave(VACULI_PC1Pos$pct.canopy, VACULI_PC1Pos$Site, na.rm=T)

#VACVIT LPR
vvi.LPR <- lmer(LPR ~ pct.canopy + (1|Site), data = VACVIT)
summary(vvi.LPR)

vvi.up.LPR <- lmer(LPR ~ pct.canopy + (1|Site), data = VACVIT_PC1Pos)
summary(vvi.up.LPR)

vvi.low.LPR <- lmer(LPR ~ pct.canopy + (1|Site), data = VACVIT_PC1Neg)
summary(vvi.low.LPR)


summary(lm(VACVIT$LPR ~ VACVIT$CanCovAvg + (VACVIT$pct.canopy - VACVIT$CanCovAvg)))
summary(lm(VACVIT_PC1Pos$LPR ~ VACVIT_PC1Pos$CanCovAvg + (VACVIT_PC1Pos$pct.canopy - VACVIT_PC1Pos$CanCovAvg)))
summary(lm(VACVIT_PC1Neg$LPR ~ VACVIT_PC1Neg$CanCovAvg + (VACVIT_PC1Neg$pct.canopy - VACVIT_PC1Neg$CanCovAvg)))

#VACVIT FSR
vvi.FSR <- lmer(FSR ~ pct.canopy + (1|Site), data = VACVIT)
summary(vvi.FSR)

vvi.up.FSR <- lmer(FSR ~ pct.canopy + (1|Site), data = VACVIT_PC1Pos)
summary(vvi.up.FSR)

vvi.low.FSR <- lmer(FSR ~ pct.canopy + (1|Site), data = VACVIT_PC1Neg)
summary(vvi.low.FSR)

summary(lm(VACVIT$FSR ~ VACVIT$CanCovAvg + (VACVIT$pct.canopy - VACVIT$CanCovAvg)))

#VACVIT BLR
vvi.BLR <- lmer(BLR ~ pct.canopy + (1|Site), data = VACVIT)
summary(vvi.BLR)

vvi.up.BLR <- lmer(BLR ~ pct.canopy + (1|Site), data = VACVIT_PC1Pos)
summary(vvi.up.BLR)

vvi.low.BLR <- lmer(BLR ~ pct.canopy + (1|Site), data = VACVIT_PC1Neg)
summary(vvi.low.BLR)

summary(lm(VACVIT$BLR ~ VACVIT$CanCovAvg + (VACVIT$pct.canopy - VACVIT$CanCovAvg)))

#VACULI LPR
vu.LPR <- lmer(LPR ~ pct.canopy + (1|Site), data = VACULI)
summary(vu.LPR)

vu.up.LPR <- lmer(LPR ~ pct.canopy + (1|Site), data = VACULI_PC1Pos)
summary(vu.up.LPR)

vu.low.LPR <- lmer(LPR ~ pct.canopy + (1|Site), data = VACULI_PC1Neg)
summary(vu.low.LPR)

summary(lm(VACULI$LPR ~ VACULI$CanCovAvg + (VACULI$pct.canopy - VACULI$CanCovAvg)))

#VACULI FSR
vu.FSR <- lmer(FSR ~ pct.canopy + (1|Site), data = VACULI)
summary(vu.FSR)

vu.up.FSR <- lmer(FSR ~ pct.canopy + (1|Site), data = VACULI_PC1Pos)
summary(vu.up.FSR)

vu.low.FSR <- lmer(FSR ~ pct.canopy + (1|Site), data = VACULI_PC1Neg)
summary(vu.low.FSR)

summary(lm(VACULI$FSR ~ VACULI$CanCovAvg + (VACULI$pct.canopy - VACULI$CanCovAvg)))

#VACULI BLR
vu.BLR <- lmer(BLR ~ pct.canopy + (1|Site), data = VACULI)
summary(vu.FSR)

vu.up.BLR <- lmer(BLR ~ pct.canopy + (1|Site), data = VACULI_PC1Pos)
summary(vu.up.FSR)

vu.low.BLR <- lmer(BLR ~ pct.canopy + (1|Site), data = VACULI_PC1Neg)
summary(vu.low.BLR)

summary(lm(VACULI$BLR ~ VACULI$CanCovAvg + (VACULI$pct.canopy - VACULI$CanCovAvg)))

