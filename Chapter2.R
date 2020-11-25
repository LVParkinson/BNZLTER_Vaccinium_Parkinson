#This file has many of the same calulations as Morphology.R
# HOWEVER it also has many of the figures I considered or submitted for 
# the final thesis and manuscript 

setwd("...")
BOTH <- read.csv("BOTH.csv")
VACULI_PC1Pos <- subset(VACULI, VACULI$PC1>0)
VACULI_PC1Neg <- subset(VACULI, VACULI$PC1 <0)
VACVIT_PC1Pos <- subset(VACVIT, VACVIT$PC1>0)
VACVIT_PC1Neg <- subset(VACVIT, VACVIT$PC1<0)


#Figure
par(mfrow=c(1,2))

pct.vu <- aggregate(VACULI[,c(14,27)], by= list(VACULI$Site), mean, na.rm=TRUE) #reproduction ~ canopy
plot(pct.vu$pct.repro~pct.vu$pct.canopy, xlab= "% canopy cover", ylab = "% blueberry ramets reproductive", xlim=c(10,75), ylim=c(0,85))
title("a", adj = .03, line = .2)
abline(lm(pct.vu$pct.repro~pct.vu$pct.canopy))
summary(lm(pct.vu$pct.repro~pct.vu$pct.canopy))

pct <- aggregate(VACVIT[,c(15,28)], by= list(VACVIT$Site), mean, na.rm=TRUE)   #reproduction ~ canopy
plot(pct$pct.repro~pct$pct.canopy, xlab= "% canopy cover", ylab = "% lingonberry ramets reproductive", xlim=c(10,75), ylim=c(0,85))
title("b", adj = .03, line = .2)
abline(lm(pct$pct.repro~pct$pct.canopy))
summary(lm(pct$pct.repro~pct$pct.canopy))







#VACULI Morphology
VACULI <- read.csv("VACULI.csv")

VACULI$Leaf..g. <- as.numeric(as.character(VACULI$Leaf..g.))
VACULI$SLA <- VACULI$SA..mm2./VACULI$Leaf..g.

plot(VACULI$SLA~VACULI$PC1, ylim= c(5000,40000))   #SLA along PC1/elevation
abline(lm(VACULI$SLA~VACULI$PC1))
summary(lm(VACULI$SLA~VACULI$PC1))


plot(VACULI$SLA~VACULI$pct.canopy)   #SLA ~ canopy at ramet level
abline(lm(VACULI$SLA~VACULI$pct.canopy + VACULI$SLA))
summary(lm(VACULI$SLA~VACULI$pct.canopy))
AIC(lm(VACULI$SLA~VACULI$pct.canopy))
test <- lm(VACULI$SLA~VACULI$pct.canopy)

VACULI$ZSLA <- (VACULI$SLA-mean(VACULI$SLA, na.rm=T))/sd(VACULI$SLA, na.rm=T)
plot(VACULI$ZSLA~VACULI$pct.canopy)   #ZSLA ~ canopy at ramet level
abline(lm(VACULI$ZSLA~VACULI$pct.canopy))
summary(lm(VACULI$ZSLA~VACULI$pct.canopy))


VACULI$CanopyMean <- ave(VACULI$pct.canopy, VACULI$Site)  #column with canopy mean by site

plot(VACULI$SLA~VACULI$CanopyMean)
abline(lm(VACULI$SLA~VACULI$CanopyMean))
summary(lm(VACULI$SLA~VACULI$CanopyMean))

plot(VACULI$ZSLA~VACULI$CanopyMean)
abline(lm(VACULI$ZSLA~VACULI$CanopyMean))
summary(lm(VACULI$ZSLA~VACULI$CanopyMean))

pct.vu <- aggregate(VACULI[,c(14,27)], by= list(VACULI$Site), mean, na.rm=TRUE) #reproduction ~ canopy
plot(pct.vu$pct.repro~pct.vu$pct.canopy, xlab= "% canopy cover", ylab = "% blueberry ramets reproductive")
abline(lm(pct.vu$pct.repro~pct.vu$pct.canopy))
summary(lm(pct.vu$pct.repro~pct.vu$pct.canopy))

pct2.vu <- aggregate(VACULI[,c(26, 27)], by= list(VACULI$Site), mean, na.rm=TRUE) #reproduction ~ canopy
plot(pct2.vu$pct.repro~pct2.vu$PC1, xlab= "PC1 Value", ylab = "% blueberry ramets reproductive")
abline(lm(pct2.vu$pct.repro~pct2.vu$PC1))
summary(lm(pct2.vu$pct.repro~pct2.vu$PC1))

#VACVIT Morphology
VACVIT <- read.csv("VACVIT.csv")

VACVIT$SLA <- VACVIT$SA..mm2./VACVIT$Leaf..g.

plot(VACVIT$SLA~VACVIT$PC1)    #SLA~PC1
abline(lm(VACVIT$SLA~VACVIT$PC1))
summary(lm(VACVIT$SLA~VACVIT$PC1))

hist(as.numeric(as.character(VACVIT$SLA)))
plot(VACVIT$SLA~VACVIT$pct.canopy)   #SLA ~ canopy at ramet level
abline(lm(VACVIT$SLA~VACVIT$pct.canopy))
summary(lm(VACVIT$SLA~VACVIT$pct.canopy))

VACVIT$SLA <- as.numeric(as.character(VACVIT$SLA))
VACVIT$ZSLA <- (VACVIT$SLA-mean(VACVIT$SLA, na.rm=T))/sd(VACVIT$SLA, na.rm=T)

plot(VACVIT$ZSLA~VACVIT$pct.canopy)   #ZSLA ~ canopy at ramet level
abline(lm(VACVIT$ZSLA~VACVIT$pct.canopy))
summary(lm(VACVIT$ZSLA~VACVIT$pct.canopy))

VACVIT$CanopyMean <- ave(VACVIT$pct.canopy, VACVIT$Site)

plot(VACVIT$SLA~VACVIT$CanopyMean)    # SLA ~ canopy/site
abline(lm(VACVIT$SLA~VACVIT$CanopyMean))
summary(lm(VACVIT$SLA~VACVIT$CanopyMean))

plot(VACVIT$ZSLA~VACVIT$CanopyMean)    # ZSLA ~ canopy/site
abline(lm(VACVIT$ZSLA~VACVIT$CanopyMean))
summary(lm(VACVIT$ZSLA~VACVIT$CanopyMean))



pct <- aggregate(VACVIT[,c(15,28)], by= list(VACVIT$Site), mean, na.rm=TRUE)   #reproduction ~ canopy
plot(pct$pct.repro~pct$pct.canopy, xlab= "% canopy cover", ylab = "% lingonberry ramets reproductive")
abline(lm(pct$pct.repro~pct$pct.canopy))
summary(lm(pct$pct.repro~pct$pct.canopy))


pct2.vvi <- aggregate(VACVIT[,c(27,28)], by= list(VACVIT$Site), mean, na.rm=TRUE)   #reproduction ~ PC1
plot(pct2.vvi$pct.repro~pct2.vvi$PC1, xlab= "PC1 Value", ylab = "% lingonberry ramets reproductive")
abline(lm(pct2.vvi$pct.repro~pct2.vvi$PC1))
summary(lm(pct2.vvi$pct.repro~pct2.vvi$PC1))

#VACULI Reproductive allocation
WellPoll <- VACULI$Pollen >= 10 
NotPoll <- VACULI$Pollen <=10

anova(lm(VACULI$X.Flwrs~VACULI$PC1)) #anova nFlwrs


VACULI$BMR <- VACULI$Berry..g./VACULI$PlantMass   #Berry:Plant mass ratio

plot(VACULI$BMR~VACULI$PC1)    #Includes all ramets
abline(lm(VACULI$BMR~VACULI$PC1))
summary(lm(VACULI$BMR~VACULI$PC1))

VACULI.Berry <- subset(VACULI, X.Berries >= 1)

plot(VACULI.Berry$BMR~VACULI.Berry$PC1)    #Includes only ramets w Berries
abline(lm(VACULI.Berry$BMR~VACULI.Berry$PC1))
summary(lm(VACULI.Berry$BMR~VACULI.Berry$PC1))

VACULI$BLR <- VACULI$Berry..g./VACULI$Leaf..g.    #Berry:Leaf mass ratio

plot(VACULI$BLR~VACULI$PC1)    
abline(lm(VACULI$BLR~VACULI$PC1))
summary(lm(VACULI$BLR~VACULI$PC1))

plot(VACULI$BLR~VACULI$pct.canopy)    
abline(lm(VACULI$BLR~VACULI$pct.canopy))
summary(lm(VACULI$BLR~VACULI$pct.canopy))

plot(VACULI$BLR~VACULI$Age)    
abline(lm(VACULI$BLR~VACULI$Age))
summary(lm(VACULI$BLR~VACULI$Age))

VACULI.Berry$BLR <- VACULI.Berry$Berry..g./VACULI.Berry$Leaf..g.    #Berry:Leaf mass ratio Berries>0

plot(VACULI.Berry$BLR~VACULI.Berry$PC1)    
abline(lm(VACULI.Berry$BLR~VACULI.Berry$PC1))
summary(lm(VACULI.Berry$BLR~VACULI.Berry$PC1))

plot(VACULI.Berry$BLR~VACULI.Berry$pct.canopy)       #BLR (>0) ~ canopy
abline(lm(VACULI.Berry$BLR~VACULI.Berry$pct.canopy))
summary(lm(VACULI.Berry$BLR~VACULI.Berry$pct.canopy))



plot(VACULI$Berry..g.~VACULI$Leaf..g.)
abline(lm(VACULI$Berry..g.~VACULI$Leaf..g.))
summary(lm(VACULI$Berry..g.~VACULI$Leaf..g.))


plot(VACULI$X.Flwrs~VACULI$PC1)        #nFlowers ~ PC1
abline(lm(VACULI$X.Flwrs~VACULI$PC1))
summary(lm(VACULI$X.Flwrs~VACULI$PC1))

plot(VACULI$X.Flwrs~VACULI$pct.canopy)        #nFlowers ~ Canopy
abline(lm(VACULI$X.Flwrs~VACULI$pct.canopy))
summary(lm(VACULI$X.Flwrs~VACULI$pct.canopy))

plot(VACULI_Low$X.Flwrs~VACULI_Low$pct.canopy)        #nFlowers ~ Canopy (Low)
abline(lm(VACULI_Low$X.Flwrs~VACULI_Low$pct.canopy))
summary(lm(VACULI_Low$X.Flwrs~VACULI_Low$pct.canopy))

plot(VACULI_High$X.Flwrs~VACULI_High$pct.canopy)        #nFlowers ~ Canopy (High)
abline(lm(VACULI_High$X.Flwrs~VACULI_High$pct.canopy))
summary(lm(VACULI_High$X.Flwrs~VACULI_High$pct.canopy))


plot(VACULI$X.Leaves~VACULI$pct.canopy)        #nLeaves ~ Canopy
abline(lm(VACULI$X.Leaves~VACULI$pct.canopy))
summary(lm(VACULI$X.Leaves~VACULI$pct.canopy))

plot((VACULI$X.Flwrs/VACULI$PlantMass)~VACULI$PC1)        #nFlowers/PlantMass ~ PC1
abline(lm((VACULI$X.Flwrs/VACULI$PlantMass)~VACULI$PC1))
summary(lm((VACULI$X.Flwrs/VACULI$PlantMass)~VACULI$PC1))

plot((VACULI$X.Flwrs/VACULI$PlantMass)~VACULI$pct.canopy)        #nFlowers/PlantMass ~ Canopy
abline(lm((VACULI$X.Flwrs/VACULI$PlantMass)~VACULI$pct.canopy))
summary(lm((VACULI$X.Flwrs/VACULI$PlantMass)~VACULI$pct.canopy))

plot(VACULI$X.Berries~VACULI$PC1)        #nBerries ~ PC1
abline(lm(VACULI$X.Berries~VACULI$PC1))
summary(lm(VACULI$X.Berries~VACULI$PC1))

plot(VACULI$X.Berries~VACULI$pct.canopy)        #nBerries ~ canopy
abline(lm(VACULI$X.Berries~VACULI$pct.canopy))
summary(lm(VACULI$X.Berries~VACULI$pct.canopy))

VACULI.Flwrs <- subset(VACULI, X.Flwrs)    #Fruit Set ~ PC1
FruitSet.Vu <- log(VACULI$X.Berries+1)/log(VACULI$X.Flwrs+1)
plot(FruitSet.Vu~VACULI$PC1, na.rm=T)
abline(lm(FruitSet.Vu~VACULI$PC1))
summary(lm(FruitSet.Vu~VACULI$PC1), na.rm=T)

FruitAvg.Vu <- aggregate(FruitSet.Vu, by= list(VACULI$Site), mean, na.rm=T)
summary(FruitAvg.Vu)


plot(VACULI_Low$FruitSet~VACULI_Low$pct.canopy)      #Low FruitSet ~ Canopy
abline(lm(VACULI_Low$FruitSet~VACULI_Low$pct.canopy))
summary(lm(VACULI_Low$FruitSet~VACULI_Low$pct.canopy))


VACULI_High$FruitSet <- log(VACULI_High$X.Berries+1)/log(VACULI_High$X.Flwrs+1)
VACULI_Low$FruitSet <- log(VACULI_Low$X.Berries+1)/log(VACULI_Low$X.Flwrs+1)


VACULI$FruitSet <- log(VACULI$X.Berries+1)/log(VACULI$X.Flwrs+1)

BOTH$FruitSet <- log(BOTH$X.Berries+1)/log(BOTH$X.Flwrs+1)

VACULI$ElevClass <- ifelse(VACULI$Elevation<300, "Low", "High")
BOTH$ElevClass <-  ifelse(BOTH$Elevation<300, "Low", "High")

summary(aov(FruitSet.Vu~VACULI$ElevClass))
boxplot(FruitSet.Vu ~ VACULI$ElevClass)

library(ggplot2)
ggplot(VACULI, aes(x=ElevClass, y=FruitSet)) + 
  geom_violin() + stat_summary(fun.y=mean, geom="point", shape=23, size=2)

ggplot(BOTH, aes(x=ElevClass, y=FruitSet, fill=Species)) + 
  geom_violin() + stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
    scale_fill_manual(values=c("Blue", "Red")) +
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(1))

plot(FruitSet.Vu~VACULI$Elevation)
abline(lm(FruitSet.Vu~VACULI$Elevation))
summary(lm(FruitSet.Vu~VACULI$Elevation))


#VACVIT Reproductive allocation

anova(lm(VACVIT$X.Flwrs~TFPC1.Vvi)) #anova nFlwrs

VACVIT$PlantMass <- as.numeric(as.character(VACVIT$PlantMass))
VACVIT$BMR <- VACVIT$Berry..g./VACVIT$PlantMass  #Berry:Plant mass ratio

plot(VACVIT$BMR~VACVIT$PC1)   #Includes all ramets
abline(lm(VACVIT$BMR~VACVIT$PC1))
summary(lm(VACVIT$BMR~VACVIT$PC1))

VACVIT.Berry <- subset(VACVIT, X.Berries >= 1)

plot(VACVIT.Berry$BMR~VACVIT.Berry$PC1)   #Includes only ramets w Berries
abline(lm(VACVIT.Berry$BMR~VACVIT.Berry$PC1))
summary(lm(VACVIT.Berry$BMR~VACVIT.Berry$PC1))

VACVIT$BLR <- VACVIT$Berry..g./VACVIT$Leaf..g.   #Berry mass : leaf mass

plot(VACVIT$BLR~VACVIT$PC1)     # BLR ~ PC1
abline(lm(VACVIT$BLR~VACVIT$PC1))
summary(lm(VACVIT$BLR~VACVIT$PC1))

plot(VACVIT$BLR~VACVIT$pct.canopy)   # BLR~canopy
abline(lm(VACVIT$BLR~VACVIT$pct.canopy))
summary(lm(VACVIT$BLR~VACVIT$pct.canopy))

plot(VACVIT$BLR~VACVIT$Age)   # BLR~Age
abline(lm(VACVIT$BLR~VACVIT$Age))
summary(lm(VACVIT$BLR~VACVIT$Age))

VACVIT.Berry$BLR <- VACVIT.Berry$Berry..g./VACVIT.Berry$Leaf..g.   #Berry mass : leaf mass, berries >0

plot(VACVIT.Berry$BLR~VACVIT.Berry$PC1)   
abline(lm(VACVIT.Berry$BLR~VACVIT.Berry$PC1))
summary(lm(VACVIT.Berry$BLR~VACVIT.Berry$PC1))

plot(VACVIT.Berry$BLR~VACVIT.Berry$pct.canopy)   
abline(lm(VACVIT.Berry$BLR~VACVIT.Berry$pct.canopy))
summary(lm(VACVIT.Berry$BLR~VACVIT.Berry$pct.canopy))


plot(VACVIT$X.Flwrs~VACVIT$PC1)   #Flwrs ~ PC1   
abline(lm(VACVIT$X.Flwrs~VACVIT$PC1))
summary(lm(VACVIT$X.Flwrs~VACVIT$PC1))

plot(VACVIT$X.Flwrs~VACVIT$pct.canopy)   #Flwrs ~ Canopy   
abline(lm(VACVIT$X.Flwrs~VACVIT$pct.canopy))
summary(lm(VACVIT$X.Flwrs~VACVIT$pct.canopy))

plot(VACVIT_Low$X.Flwrs~VACVIT_Low$pct.canopy)   #Flwrs ~ Canopy  (Low) 
abline(lm(VACVIT_Low$X.Flwrs~VACVIT_Low$pct.canopy))
summary(lm(VACVIT_Low$X.Flwrs~VACVIT_Low$pct.canopy))

plot(VACVIT_High$X.Flwrs~VACVIT_High$pct.canopy)   #Flwrs ~ Canopy  (High) 
abline(lm(VACVIT_High$X.Flwrs~VACVIT_High$pct.canopy))
summary(lm(VACVIT_High$X.Flwrs~VACVIT_High$pct.canopy))

plot(VACVIT$X.Leaves~VACVIT$pct.canopy)   #nLeaves ~ Canopy   
abline(lm(VACVIT$X.Leaves~VACVIT$pct.canopy))
summary(lm(VACVIT$X.Leaves~VACVIT$pct.canopy))

plot((VACVIT$X.Flwrs/VACVIT$PlantMass)~VACVIT$PC1)   #Flwrs/plantMass ~ PC1   
abline(lm((VACVIT$X.Flwrs/VACVIT$PlantMass)~VACVIT$PC1))
summary(lm((VACVIT$X.Flwrs/VACVIT$PlantMass)~VACVIT$PC1))

plot((VACVIT$X.Flwrs/VACVIT$PlantMass)~VACVIT$pct.canopy)   #Flwrs/plantMass ~ Canopy   
abline(lm((VACVIT$X.Flwrs/VACVIT$PlantMass)~VACVIT$pct.canopy))
summary(lm((VACVIT$X.Flwrs/VACVIT$PlantMass)~VACVIT$pct.canopy))

plot(VACVIT$X.Berries~VACVIT$PC1)   #Berries ~ PC1   
abline(lm(VACVIT$X.Berries~VACVIT$PC1))
summary(lm(VACVIT$X.Berries~VACVIT$PC1))

plot(VACVIT$X.Berries~VACVIT$pct.canopy)   #Berries ~ Canopy   
abline(lm(VACVIT$X.Berries~VACVIT$pct.canopy))
summary(lm(VACVIT$X.Berries~VACVIT$pct.canopy))

VACVIT$FruitSet <- log(VACVIT$X.Berries+1)/log(VACVIT$X.Flwrs+1)

FruitAvg.Vvi <- aggregate(VACVIT$FruitSet, by= list(VACVIT$Site), mean, na.rm=T)
summary(FruitAvg.Vvi)

plot(VACVIT$FruitSet~VACVIT$PC1)   #VACVIT FruitSet ~ PC1   
abline(lm(VACVIT$FruitSet~VACVIT$PC1))
summary(lm(VACVIT$FruitSet~VACVIT$PC1))


plot(VACVIT$FruitSet~log(VACVIT$PollenAv+1))
abline(lm(VACVIT$FruitSet~log(VACVIT$PollenAv+1)))
summary(lm(VACVIT$FruitSet~log(VACVIT$PollenAv+1)))

plot(VACVIT$FruitSet~log(VACVIT$TFR+1))         #FruitSet ~ TFR
abline(lm(VACVIT$FruitSet~log(VACVIT$TFR+1)))
summary(lm(VACVIT$FruitSet~log(VACVIT$TFR+1)))

VACVIT_Low$FruitSet <-  log(VACVIT_Low$X.Berries+1)/log(VACVIT_Low$X.Flwrs+1)
VACVIT_High$FruitSet <-  log(VACVIT_High$X.Berries+1)/log(VACVIT_High$X.Flwrs+1)

plot(VACVIT_Low$FruitSet~VACVIT_Low$pct.canopy)      #Low FruitSet ~ Canopy
abline(lm(VACVIT_Low$FruitSet~VACVIT_Low$pct.canopy))
summary(lm(VACVIT_Low$FruitSet~VACVIT_Low$pct.canopy))

plot(VACVIT_High$FruitSet~VACVIT_High$pct.canopy)      #High FruitSet ~ Canopy
abline(lm(VACVIT_High$FruitSet~VACVIT_High$pct.canopy))
summary(lm(VACVIT_High$FruitSet~VACVIT_High$pct.canopy))


#Site
plot(VACULI$pct.canopy~VACULI$PC1)
abline(lm(VACULI$pct.canopy~VACULI$PC1))
summary(lm(VACULI$pct.canopy~VACULI$PC1))

#Boxplots
VACVIT_PC1Neg$category <- "negative"
VACVIT_PC1Pos$category <- "positive"
VACULI_PC1Pos$category <- "positive"
VACULI_PC1Neg$category <- "negative"

VACVIT_PC1Neg$Pollen <- rowMeans(VACVIT_PC1Neg[,21:23], na.rm=T)
VACVIT_PC1Pos$Pollen <- rowMeans(VACVIT_PC1Pos[,21:23], na.rm=T)

BOTH_category <- rbind(VACULI_PC1Neg[,c("Species", "Site", "X.Berries", "X.Flwrs", "Pollen", "category")], 
                       VACULI_PC1Pos[,c("Species", "Site", "X.Berries", "X.Flwrs", "Pollen", "category")], 
                       VACVIT_PC1Neg[,c("Species", "Site", "X.Berries", "X.Flwrs", "Pollen", "category")], 
                       VACVIT_PC1Pos[,c("Species", "Site", "X.Berries", "X.Flwrs", "Pollen", "category")])

BOTH_category$Species <- factor(BOTH_category$Species, c("VACULI", "VACVIT"))
BOTH_category$category <- factor(BOTH_category$category, c("negative", "positive"))
BOTH_category[28,4] <- 14
BOTH_category$FruitSet <- BOTH_category$X.Berries/BOTH_category$X.Flwrs

# Boxplot - no beeswarm
par(mfrow=c(1,3))
boxplot(Pollen~category*Species, data= BOTH_category, ylab = "", 
        las=3, cex.lab=1.7, cex.axis = 1, col= "#0000ff22", outline=T, xaxt='n')
axis(side = 1, at =1:4, labels = c("Vu Low", "Vu Up", "Vvi Low", "Vvi Up"), las=2, tck=0)
title("a", adj = .03, line = .2)
title(ylab = "Pollen tetrads per stigma", cex.lab = 1.7, line = 2.1)
boxplot(FruitSet~category*Species, data= BOTH_category, ylab = "", ylim=c(0,1.2),
        las=3, cex.lab=1.7, cex.axis = 1, col= "#0000ff22", outline=T, xaxt='n')
axis(side = 1, at =1:4, labels = c("Vu Low", "Vu Up", "Vvi Low", "Vvi Up"), las=2, tck=0)
title("b", adj = .03, line = .2)
title(ylab = "Fruit Set", cex.lab = 1.7, line = 2.1)
boxplot(X.Berries~category*Species, data= BOTH_category, ylab= "", 
        las=3, cex.lab=1.7, cex.axis = 1, col= "#0000ff22", outline=T, xaxt='n')
axis(side = 1, at =1:4, labels = c("Vu Low", "Vu Up", "Vvi Low", "Vvi Up"), las=2, tck=0)
title("c", adj = .03, line = .2)
title(ylab = "Berries per ramet", cex.lab = 1.7, line = 2.1)

#Figure 4 in Reproduction Chapter
par(mfrow=c(1,3))
library(beeswarm)
par(mgp=c(1.5,.5,0))
beeswarm(Pollen~category*Species, data= BOTH_category, method = "swarm", pch = 16, ylab = "Pollen Load", xlab = '', yaxt="n",
         labels = c("Vu Low", "Vu Up", "Vvi Low", "Vvi Up"), cex.lab=1.7, cex.axis = 1.5, las=2, col = "grey")
axis(2, cex.axis = 1)
boxplot(Pollen~category*Species, data= BOTH_category, add=T, col= "#0000ff22", names = '', outline=F)
title("a", adj = .03, line = .2)

beeswarm(FruitSet~category*Species, data= BOTH_category, method = "swarm", pch = 16, ylab = "Fruit Set (Total berries : total flowers)", xlab = "", yaxt="n",
         labels = c("Vu low", "Vu up", "Vvi low", "Vvi up") , cex.lab = 1.7, cex.axis = 1.5, las=2, ylim=c(0,1), col = "grey")
axis(2, cex.axis = 1)
boxplot(FruitSet~category*Species, data= BOTH_category, add=T,col= "#0000ff22", names = '', outline=F)
title("b", adj = .03, line = .2)

beeswarm(X.Berries~category*Species, data= BOTH_category, method = "swarm", pch = 16, ylab = "# Berries", xlab = '', yaxt="n",
         labels = c("Vu low", "Vu up", "Vvi low", "Vvi up"), cex.lab=1.7, cex.axis = 1.5, las=2, col = "grey")
axis(2, cex.axis = 1)
boxplot(X.Berries~category*Species, data= BOTH_category, add = T, col= "#0000ff22", names = '', outline=F)
title("c", adj = .03, line = .2)

#Figure 4 in Reproduction Chapter - No raw data
par(mfrow=c(1,1))
boxplot(Pollen~category*Species, data= BOTH_category, add=T, col= "#0000ff22")
title("a", adj = .03, line = .2)

beeswarm(FruitSet~category*Species, data= BOTH_category, method = "swarm", pch = 16, ylab = "Fruit Set (Total berries : total flowers)", xlab = "", yaxt="n",
         labels = c("Vu low", "Vu up", "Vvi low", "Vvi up") , cex.lab = 1.7, cex.axis = 1.5, las=2, ylim=c(0,1), col = "grey")
axis(2, cex.axis = 1)
boxplot(FruitSet~category*Species, data= BOTH_category, add=T,col= "#0000ff22", names = '', outline=F)
title("b", adj = .03, line = .2)

beeswarm(X.Berries~category*Species, data= BOTH_category, method = "swarm", pch = 16, ylab = "# Berries", xlab = '', yaxt="n",
         labels = c("Vu low", "Vu up", "Vvi low", "Vvi up"), cex.lab=1.7, cex.axis = 1.5, las=2, col = "grey")
axis(2, cex.axis = 1)
boxplot(X.Berries~category*Species, data= BOTH_category, add = T, col= "#0000ff22", names = '', outline=F)
title("c", adj = .03, line = .2)


median(VACVIT_PC1Neg$pct.canopy, na.rm=T)
median(VACULI_PC1Neg$pct.canopy, na.rm=T)

par(mfrow=c(2,2))

plot(VACULI$X.Flwrs~VACULI$pct.canopy, xlab = "% canopy cover", ylab = "Blueberry flowers per ramet")        #nFlowers ~ Canopy
title("a", adj = .03, line = .2)
abline(lm(VACULI$X.Flwrs~VACULI$pct.canopy))
summary(lm(VACULI$X.Flwrs~VACULI$pct.canopy))

plot(VACVIT$X.Flwrs~VACVIT$pct.canopy, xlab= "% canopy cover", ylab = "Lingonberry flowers per ramet", ylim = c(0,140))   #Flwrs ~ Canopy   
title("b", adj = .03, line = .2)
abline(lm(VACVIT$X.Flwrs~VACVIT$pct.canopy))
summary(lm(VACVIT$X.Flwrs~VACVIT$pct.canopy))

plot(VACULI$X.Berries~VACULI$pct.canopy, xlab= "% canopy cover", ylab = "Blueberries per ramet")        #nBerries ~ canopy
title("c", adj = .03, line = .2)
abline(lm(VACULI$X.Berries~VACULI$pct.canopy))
summary(lm(VACULI$X.Berries~VACULI$pct.canopy))

plot(VACVIT$X.Berries~VACVIT$pct.canopy, xlab = "% canopy cover", ylab = "Lingonberries per ramet", ylim=c(0,35))   #Berries ~ Canopy   
title("d", adj = .03, line = .2)
abline(lm(VACVIT$X.Berries~VACVIT$pct.canopy))
summary(lm(VACVIT$X.Berries~VACVIT$pct.canopy))



#-------------------------------------------------

plot(VACULI_PC1Neg$Stem..g.~VACULI_PC1Neg$pct.canopy)        #Stem mass ~ Canopy (PC1Neg)
abline(lm(VACULI_PC1Neg$Stem..g.~VACULI_PC1Neg$pct.canopy))
summary(lm(VACULI_PC1Neg$Stem..g.~VACULI_PC1Neg$pct.canopy))

plot(VACULI_PC1Pos$Stem..g.~VACULI_PC1Pos$pct.canopy)        #Stem mass ~ Canopy (PC1Pos)
abline(lm(VACULI_PC1Pos$Stem..g.~VACULI_PC1Pos$pct.canopy))
summary(lm(VACULI_PC1Pos$Stem..g.~VACULI_PC1Pos$pct.canopy))

#VACVIT
plot(VACVIT_PC1Neg$Stem..g.~VACVIT_PC1Neg$pct.canopy)        #Stem mass ~ Canopy (PC1Neg)
abline(lm(VACVIT_PC1Neg$Stem..g.~VACVIT_PC1Neg$pct.canopy))
summary(lm(VACVIT_PC1Neg$Stem..g.~VACVIT_PC1Neg$pct.canopy))

plot(VACVIT_PC1Pos$Stem..g.~VACVIT_PC1Pos$pct.canopy)        #Stem mass ~ Canopy (PC1Pos)
abline(lm(VACVIT_PC1Pos$Stem..g.~VACVIT_PC1Pos$pct.canopy))
summary(lm(VACVIT_PC1Pos$Stem..g.~VACVIT_PC1Pos$pct.canopy))

summary(lm(VACVIT$Stem..g.~VACVIT$pct.canopy))
summary(lm(VACULI$Stem..g.~VACULI$pct.canopy))
