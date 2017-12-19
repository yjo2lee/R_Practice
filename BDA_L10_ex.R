#Ch10. Hypothesis Testing_Categorical Data
# summary
# contingency table - two sample test for binomial proportions / exact test for contingency table
# McNamar's test - version of paired proportion tests
# sample sizes and Power(trialsize?)
# R*C contingency table
# Kappa stat.

# 10.9_12_gastroenterology
# 10.9_McNemar's test
du_treat<-rbind(c(89,5), c(16, 90))
mcnemar.test(du_treat)
du_treat_male<- rbind(c(52,4), c(9, 35))
mcnemar.test(du_treat_male)
# 10.12
# discordant pairs: 13 -> nd/4 < 5
# normal approximation is not valid
# Use proportions-Exact test
binom.test(4,13, p = 0.5, alternative = c("two.sided"), conf.level = 0.95)


# 10.37_39
load("/Users/sjlee/Desktop/school4_2/BioDataAnalysis/Data/HORMONE.DAT.rdata")
Bilsecdiff<-hormone$Bilsecpt-hormone$Bilsecpr
Pansecdiff<-hormone$Pansecpt-hormone$Pansecpr
# add column to a list
hormone["Bilsecdiff"] <- NA
hormone["Bilsecdiff"] <- Bilsecdiff
hormone["Pansecdiff"] <- NA
hormone["Pansecdiff"] <- Pansecdiff

Pansec.2<-select(filter(hormone, Hormone == 2), Dose, Pansecdiff)
Pansec.3<-select(filter(hormone, Hormone == 3), Dose, Pansecdiff)
Pansec.4<-select(filter(hormone, Hormone == 4), Dose, Pansecdiff)
Pansec.5<-select(filter(hormone, Hormone == 5), Dose, Pansecdiff)

Bilsec.2<-select(filter(hormone, Hormone == 2), Dose, Bilsecdiff)
Bilsec.3<-select(filter(hormone, Hormone == 3), Dose, Bilsecdiff)
Bilsec.4<-select(filter(hormone, Hormone == 4), Dose, Bilsecdiff)
Bilsec.5<-select(filter(hormone, Hormone == 5), Dose, Bilsecdiff)

# counting hens_increasement>0 or <0 using length() and which() per hormone
# need correction _ shorter one
length(which(Pansec.2$Pansecdiff>0))
length(which(Pansec.2$Pansecdiff<0))

length(which(Bilsec.2$Bilsecdiff>0))
length(which(Bilsec.2$Bilsecdiff<0))

# making contingency table of bilsec.
# RxC contingency table -> chi-square test (H0: p1 = p2 = p3 = p4 vs H1: not H0)
Pansecdiff_cgtable <- rbind(c(3, 28, 7, 34), c(20, 32, 8, 23))
chisq.test(Pansecdiff_cgtable)

Bilsecdiff_cgtable<- rbind(c(5, 81, 9, 68), c(18, 55, 25, 47))
chisq.test(Bilsecdiff_cgtable)

# 10.40
# Use trend test using Chisquare test -> pi= alpha + beta*score
# H0: beta = 0, H1: beta =! 0, pi: binary
# Regard as pi continuous
table(Pansec.2[["Dose"]])  #show how to use table()

Pansec.3["increasing"] <- NA
Pansec.3["increasing"] <- (Pansec.3[["Pansecdiff"]]>0)
Pansec.4["increasing"] <- NA
Pansec.4["increasing"] <- (Pansec.4[["Pansecdiff"]]>0)
Pansec.5["increasing"] <- NA
Pansec.5["increasing"] <- (Pansec.5[["Pansecdiff"]]>0)

##hormone2
with(Pansec.2, table(Dose, increasing))
pansec.2_tt<-rbind(c(1,2), c(8, 12))
pansec.2x<-pansec.2_tt[1,]
pannn2<-colSums(pansec.2_tt)
prop.trend.test(pansec.2x, pannn2, score=c(7.5,15))

##hormone3
with(Pansec.3, table(Dose, increasing))
pansec.3_tt<-rbind(c(2,3,6,5,6,6), c(4, 14, 19, 20, 28, 52))
pansec.3x<-pansec.3_tt[1,]
pannn3<-colSums(pansec.3_tt)
prop.trend.test(pansec.3x, pannn3, score=c(0.05,0.1,0.2,0.4,0.8,3.2))

##hormone4
with(Pansec.4, table(Dose, increasing))
pansec.4_tt<-rbind(c(3,0,1,2,1), c(17,5,3,3,3))
pansec.4x<-pansec.4_tt[1,]
pannn4<-colSums(pansec.4_tt)
prop.trend.test(pansec.4x, pannn4, score=c(0.5, 2.5, 5, 12.5, 20))

##hormone5
with(Pansec.5, table(Dose, increasing))
pansec.5_tt<-rbind(c(1,2,0,1,1,3,1,3,6,1,8,0,7), c(5,7,4,7,6,4,6,7,19,5,19,1,6))
pansec.5x<-pansec.5_tt[1,]
pannn5<-colSums(pansec.5_tt)
prop.trend.test(pansec.5x, pannn5, score=c(0.4, 0.5, 0.8, 1,1.6, 2, 3.2, 4, 8, 9.6, 12, 16, 24))

#10.41
Bilsec.2["increasing"] <- NA
Bilsec.2["increasing"] <- (Bilsec.2[["Bilsecdiff"]]>0)
Bilsec.3["increasing"] <- NA
Bilsec.3["increasing"] <- (Bilsec.3[["Bilsecdiff"]]>0)
Bilsec.4["increasing"] <- NA
Bilsec.4["increasing"] <- (Bilsec.4[["Bilsecdiff"]]>0)
Bilsec.5["increasing"] <- NA
Bilsec.5["increasing"] <- (Bilsec.5[["Bilsecdiff"]]>0)

##hormone2
with(Bilsec.2, table(Dose, increasing))
bilsec.2_tt<-rbind(c(1,4), c(13,17))
bilsec.2x<-bilsec.2_tt[1,]
bilnn2<-colSums(bilsec.2_tt)
prop.trend.test(bilsec.2x, bilnn2, score=c(7.5, 15))

##hormone3
with(Bilsec.3, table(Dose, increasing))
bilsec.3_tt<-rbind(c(1,4,8,14,24,30), c(5,13,17,11,10,28))
bilsec.3x<-bilsec.3_tt[1,]
bilnn3<-colSums(bilsec.3_tt)
prop.trend.test(bilsec.3x, bilnn3, score=c(0.05,0.1,0.2,0.4,0.8,3.2))

##hormone4
with(Bilsec.4, table(Dose, increasing))
bilsec.4_tt<-rbind(c(6,0,2,1,0), c(14,5,2,4,4))
bilsec.4x<-bilsec.4_tt[1,]
bilnn4<-colSums(bilsec.4_tt)
prop.trend.test(bilsec.4x, bilnn4, score=c(0.5, 2.5, 5, 12.5, 20))

##hormone5
with(Bilsec.5, table(Dose, increasing))
bilsec.5_tt<-rbind(c(1,3,1,0,5,4,4,9,16,2,17,1,5), c(5,6,3,8,2,3,3,1,9,4,10,0,8))
bilsec.5x<-bilsec.5_tt[1,]
bilnn5<-colSums(bilsec.5_tt)
prop.trend.test(bilsec.5x, bilnn5, score=c(0.4, 0.5, 0.8, 1,1.6, 2, 3.2, 4, 8, 9.6, 12, 16, 24))

#10.79

female<-c(549, 2476, 4753, 4621, 2245, 518)
male<-c(318, 2245, 4621, 4753, 2476, 549)
gender_dist<-rbind(male, female)
chisq.test((gender_dist))
