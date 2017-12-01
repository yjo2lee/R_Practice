#11.1_7
reticulocytes<-c(3.6, 2.0, 0.3, 0.3, 0.2, 3.0, 0.0, 1.0, 2.2)
lymphocytes <- c(1700, 3078, 1820, 2706, 2086, 2299, 676, 2088, 2013)
anemia_data<-cbind(reticulocytes, lymphocytes)
lm.anemia<- lm(lymphocytes~reticulocytes)
summary(lm.anemia)
#Rsquare<- SSR/SSTO
#Rsquare can be thought of as the proportion 
#of the variance of y that is explained by x 
# SSR = b*Lxy
Lxy<- 180750/112.1
cor.test(reticulocytes, lymphocytes)
ret_cor = 0.2235613; z_ret = 0.5*log((1+ret_cor)/(1-ret_cor))

# 11.35-36
# Hepatic Disease
library(tidyverse)
load("/Users/sjlee/Desktop/school4_2/BioDataAnalysis/Data/HORMONE.DAT.rdata")

# make column having difference
Bilsecdiff<-hormone$Bilsecpt-hormone$Bilsecpr
Pansecdiff<-hormone$Pansecpt-hormone$Pansecpr

#add column to a list
hormone["Bilsecdiff"] <- NA
hormone["Bilsecdiff"] <- Bilsecdiff
hormone["Pansecdiff"] <- NA
hormone["Pansecdiff"] <- Pansecdiff

#Bilsec &Dose by hormone_using select(), filter()
Bilsec.2<-select(filter(hormone, Hormone == 2), Dose, Bilsecdiff)
Bilsec.3<-select(filter(hormone, Hormone == 3), Dose, Bilsecdiff)

#Bilsec & Dose by hormone_different method
Bilsec.4<-hormone$Bilsecdiff[which(hormone["Hormone"]==4)]
Bilsec.5<-hormone$Bilsecdiff[which(hormone["Hormone"]==5)]
Hormone.4<-hormone$Dose[which(hormone["Hormone"]==4)]
Hormone.5<-hormone$Dose[which(hormone["Hormone"]==5)]

#lin.reg.analysis_Bilsec with hormone
lm.Bilsec.2<- lm(Bilsec.2$Bilsecdiff~Bilsec.2$Dose)
lm.Bilsec.3<- lm(Bilsec.3$Bilsecdiff~Bilsec.3$Dose)
lm.Bilsec.4<- lm(Bilsec.4~Hormone.4)
lm.Bilsec.5<- lm(Bilsec.5~Hormone.5)

#Pansec by hormone 
Pansec.2<-hormone$Pansecdiff[which(hormone["Hormone"]==2)]
Pansec.3<-hormone$Pansecdiff[which(hormone["Hormone"]==3)]
Pansec.4<-hormone$Pansecdiff[which(hormone["Hormone"]==4)]
Pansec.5<-hormone$Pansecdiff[which(hormone["Hormone"]==5)]

#lin.reg.analysis_Pansec with hormone
lm.Pansec.2<- lm(Pansec.2~Hormone.2)
lm.Pansec.3<- lm(Pansec.3~Hormone.3)
lm.Pansec.4<- lm(Pansec.4~Hormone.4)
lm.Pansec.5<- lm(Pansec.5~Hormone.5)

#11.65
#age vs. days abstinent from smoking
load("/Users/sjlee/Desktop/school4_2/BioDataAnalysis/Data/SMOKE.DAT.rdata")
cor.test(smoke$Age, smoke$Day_abs, method = "spearman")
#11.66
#Min_last vs. days abstinent from smoking
cor.test(smoke$Min_last, smoke$Day_abs, method = "spearman")
#11.67
#co vs. days abstinent from smoking
cor.test(smoke$co, smoke$Day_abs, method = "spearman")
n<-length(smoke$Day_abs)
r1<-rank(smoke$Day_abs); P1<-r1/(n+1); H1<-qnorm(P1)
r2<-rank(smoke$co); P2<-r2/(n+1); H2<-qnorm(P2)
r_h<- cor(H1, H2)
r_h2<- r_h*(1+(1-r_h^2)/(2*(n-4)))
z_h<-0.5*log((1+r_h2)/(1-r_h2))
z_h.L<-z_h - 1.96/sqrt(n-3); z_h.U<-z_h + 1.96/sqrt(n-3)
r_h.L<-(exp(2*(z_h.L))-1)/(exp(2*(z_h.L))+1); r_h.U<-(exp(2*(z_h.U))-1)/(exp(2*(z_h.U))+1)
r_s.L<-(6/pi)*asin(r_h.L/2); r_s.U<-(6/pi)*asin(r_h.U/2)
r_s.L; r_s.U




