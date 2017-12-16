# Chapter_summary

#setting data
estriol<-read.table("/Users/sjlee/Desktop/school4_2/BioDataAnalysis/Data/estriol.txt")
colnames(estriol)<-c("i","x","y")
head(estriol)

# make a dot plot
library(ggplot2)
estriol.plot <- qplot(x, y, data = estriol, alpha = I(0.5))
print(estriol.plot)

# linear reg.
lm.estriol<-lm(y~x, data=estriol)
summary(lm.estriol)

# fitted line+dotplot
coef.estriol<- coef(lm.estriol)   #When using coef.
fitted.line<-estriol.plot + 
        geom_abline(intercept = coef.estriol[1], slope = coef.estriol[2])

# anova test(H0: beta1 = 0: using Ftest)
anova(lm.estriol)

# correlation coefficient
r<-cor(estriol$x,estriol$y)

# correlation coefficient H0: rho = 0)
cor.test(estriol$x, estriol$y)

# z transformation


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

# 11.35
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

#Bilsec &Dose by hormone_using select(), filter()_hormone2,3
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
summary(lm.Bilsec.2); summary(lm.Bilsec.3); summary(lm.Bilsec.4); summary(lm.Bilsec.5)

#Each plot of residusal agianst x_bilsec
bilsec2.plot<-qplot(Bilsec.2$Dose, resid(lm.Bilsec.2))
bilsec3.plot<-qplot(Bilsec.3$Dose, resid(lm.Bilsec.3))
bilsec4.plot<-qplot(Hormone.4, resid(lm.Bilsec.4))
bilsec5.plot<-qplot(Hormone.5, resid(lm.Bilsec.5))

#plots of residusal agianst x_bilsec in one figure
bilsec.plot<-par(mfrow = c(2,2))
plot(Bilsec.2$Dose, resid(lm.Bilsec.2))
plot(Bilsec.3$Dose, resid(lm.Bilsec.3))
plot(Hormone.4, resid(lm.Bilsec.4))
plot(Hormone.5, resid(lm.Bilsec.5))

#Pansec by hormone 
Pansec.2<-select(filter(hormone, Hormone == 2), Dose, Pansecdiff)
Pansec.3<-select(filter(hormone, Hormone == 3), Dose, Pansecdiff)
Pansec.4<-select(filter(hormone, Hormone == 4), Dose, Pansecdiff)
Pansec.5<-select(filter(hormone, Hormone == 5), Dose, Pansecdiff)

#lin.reg.analysis_Pansec with hormone
lm.Pansec.2<- lm(Pansec.2$Pansecdiff~Pansec.2$Dose)
lm.Pansec.3<- lm(Pansec.3$Pansecdiff~Pansec.3$Dose)
lm.Pansec.4<- lm(Pansec.4$Pansecdiff~Pansec.4$Dose)
lm.Pansec.5<- lm(Pansec.5$Pansecdiff~Pansec.5$Dose)

#plot of residusal agianst x_pansec
pansec.plot<-par(mfrow = c(2,2))
plot(Pansec.2$Dose, resid(lm.Pansec.2))
plot(Pansec.3$Dose, resid(lm.Pansec.3))
plot(Pansec.4$Dose, resid(lm.Pansec.4))
plot(Pansec.5$Dose, resid(lm.Pansec.5))

#PH levels(11.36)
# make column having difference
Bilphdiff<-hormone$Bilphpt-hormone$Bilphpr
Panphdiff<-hormone$Panphpt-hormone$Panphpr

#add column to a list
hormone["Bilphdiff"] <- NA
hormone["Bilphdiff"] <- Bilphdiff
hormone["Panphdiff"] <- NA
hormone["Panphdiff"] <- Panphdiff

#Bilsec &Dose by hormone
Bilph.2<-select(filter(hormone, Hormone == 2), Dose, Bilphdiff)
Bilph.3<-select(filter(hormone, Hormone == 3), Dose, Bilphdiff)
Bilph.4<-select(filter(hormone, Hormone == 4), Dose, Bilphdiff)
Bilph.5<-select(filter(hormone, Hormone == 5), Dose, Bilphdiff)


#lin.reg.analysis_Bilsec with hormone
lm.Bilph.2<- lm(Bilph.2$Bilphdiff~Bilph.2$Dose)
lm.Bilph.3<- lm(Bilph.3$Bilphdiff~Bilph.3$Dose)
lm.Bilph.4<- lm(Bilph.4$Bilphdiff~Bilph.4$Dose)
lm.Bilph.5<- lm(Bilph.5$Bilphdiff~Bilph.5$Dose)

#plots of residusal agianst x_bilsec in one figure
bilph.plot<-par(mfrow = c(2,2))
plot(Bilph.2$Dose, resid(lm.Bilph.2))
plot(Bilph.3$Dose, resid(lm.Bilph.3))
plot(Bilph.4$Dose, resid(lm.Bilph.4))
plot(Bilph.5$Dose, resid(lm.Bilph.5))

#Panph by hormone 
Panph.2<-select(filter(hormone, Hormone == 2), Dose, Panphdiff)
Panph.3<-select(filter(hormone, Hormone == 3), Dose, Panphdiff)
Panph.4<-select(filter(hormone, Hormone == 4), Dose, Panphdiff)
Panph.5<-select(filter(hormone, Hormone == 5), Dose, Panphdiff)

#lin.reg.analysis_Panph with hormone
lm.Panph.2<- lm(Panph.2$Panphdiff~Panph.2$Dose)
lm.Panph.3<- lm(Panph.3$Panphdiff~Panph.3$Dose)
lm.Panph.4<- lm(Panph.4$Panphdiff~Panph.4$Dose)
lm.Panph.5<- lm(Panph.5$Panphdiff~Panph.5$Dose)

#plot of residusal agianst x
panph.plot<-par(mfrow = c(2,2))
plot(Panph.2$Dose, resid(lm.Panph.2))
plot(Panph.3$Dose, resid(lm.Panph.3))
plot(Panph.4$Dose, resid(lm.Panph.4))
plot(Panph.5$Dose, resid(lm.Panph.5))

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




