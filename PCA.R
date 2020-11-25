setwd("...")
BOTH <- read.csv("BOTH.csv")
VACVIT <- read.csv("VACVIT.csv")

# Average of environmental measures of the sites
AvTemp.vvi <- as.numeric(VACVIT$AvTemp18)
AvMoist.vvi <- as.numeric(VACVIT$AvMoist18)
ALD.vvi <- as.numeric(VACVIT$ALD)
CanCov.vvi <- VACVIT$pct.canopy 
Age.vvi <- VACVIT$Age
Elev.vvi <- VACVIT$Elevation
Site.vvi<- VACVIT$Site

PCA_Test <- as.data.frame(cbind(AvTemp.vvi, AvMoist.vvi, ALD.vvi, Age.vvi, Elev.vvi,CanCov.vvi, Site.vvi))
PCA_Test_Avg <- aggregate(PCA_Test[1:6], by = list(PCA_Test$Site.vvi), mean, na.rm=TRUE)
PCA_Test_Avg
PCA_Test_Avg[12,2] <- mean(PCA_Test_Avg$AvTemp.vvi, na.rm=T)
PCA_Test_Avg[12,3] <- mean(PCA_Test_Avg$AvMoist.vvi, na.rm=T)
PCA_Test_Avg[11,7] <- mean(PCA_Test_Avg$CanCov.vvi, na.rm=T)
PCA_Test_Avg

PCA_testZ <- apply(PCA_Test_Avg[,2:7], 2, scale)
#PCA_testZ_Full <- PCA_testZ[complete.cases(PCA_testZ),]

PCA_testZ

PCA <- princomp(PCA_testZ[,c(1:5)], cor=F) #cor=F indicates PCA done on a covariance matrix
summary(PCA)
screeplot(PCA, type=c('lines'))
Scores <- as.data.frame(PCA$scores[,1:2], row.names= 
                          c("BFY1" , "BFY10","BFY6","GSI1", "GSI2", "GSM3", "GSM4", "MDI5", "UP4A","UP4B", "UP4C",
                          "UP4D",  "WCM1", "WCM3","WCM4", "WDI5", "WDI6"))

color<- ifelse(PCA$scores[,1]> 0,"black","grey")
plot(Scores[,1], Scores[,2], col=color, 
     pch=19, cex=3, xlab="PC1 - 45% - Elevation, Soil Moisture, Active Layer Depth", 
     ylab = "PC2 - 34% - Time Since Fire, Soil Temperature", ylim=c(-2,3))
text(Scores[,1], Scores[,2], labels=row.names(Scores), cex=1, pos=3)

Scores
PCA$loadings


#Cronbach's alpha - as suggested by reviewer 1
library(psych)

alpha(PCA_testZ, check.keys = T)
#used check.key=T to change negatives
# probably a more reliable way to do this
# raw_alpha = 0.64     
#The work of Nunnally (1978) provided that the lower cut-off (i.e., 0.70) 
#was appropriate in the early stages of research (i.e., exploratory) 
#for basic research (0.80 or higher) and applied research (0.90 or higher) 
#(Lance et al., 2006)
#George and Mallery (2003) suggest a tiered approach consisting of the following:
# “≥ .9 – Excellent, ≥ .8 – Good, ≥ .7 – Acceptable, ≥ .6 – Questionable, 
# ≥ .5 – Poor, and ≤ .5 – Unacceptable” (p. 231).

install.packages("psy")
library(psy)
cronbach(PCA_testZ)

Zplus2 <- PCA_testZ+2
cronbach(Zplus2)
alpha(Zplus2[c(1:2,4:6,8:17),], check.keys = T)

