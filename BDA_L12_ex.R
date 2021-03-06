# Chapter12_ㅡmultiple inference
# Anova
FEF<-data.frame(rbind(cbind(1,rnorm(200,3.78,0.79)),
                      cbind(2,rnorm(200,3.30,0.77)),
                      cbind(3,rnorm(50 ,3.32,0.86)),
                      cbind(4,rnorm(200,3.23,0.78)),
                      cbind(5,rnorm(200,2.73,0.81)),
                      cbind(6,rnorm(200,2.59,0.82))))
colnames(FEF)<-c("group","fef")
boxplot(fef~group, data=FEF)

# Overall Ftest for One-way ANOVA
aov(fef~as.factor(group), data=FEF)
anova(lm(fef~as.factor(group), data=FEF))

# Comparison of specific group in Anova
# t-test for comparison of pairs of groups
# 1) LSD_method
pairwise.t.test(FEF$fef, FEF$group, p.ad="none")

# 2) Bonferroni approach
pairwise.t.test(FEF$fef,FEF$group,p.adj="bonf")

# False Discovery Rate
x<- rnorm(50, mean = c(rep(0, 25), rep(3, 25))) #mean이 각각 0, 3인 RV 25개, 25개 만들기
p<- 2* pnorm(sort(-abs(x))) # -abs(x) 를 sort하고 각각의 normal prob.

p.raw<- p
p.bonf<- p.adjust(p, "bonferroni")   # adjust할 때 bonferroni 이용
p.bh<-p.adjust(p, "BH")   #BH: Benjamini & Hochberg method

par(mfrow = c(1,3))   #grpah 분할해서 한 figure에 넣기
plot(p.raw);abline(h=0.05)   #abline-> h: yvalue
plot(p.bonf);abline(h = 0.05)
plot(p.bh);abline(h = 0.05)

#Kruskal-Walis test
drugs<-c(rep("Indomethacin", 6), rep("Aspirin", 6), rep("Piroxicam",6), rep("BW755C", 6))
score<-c(2,3,3,3,3,0,
         1,3,1,2,2,3,
         3,1,2,1,3,3,
         1,0,0,0,0,-1)
oscular<-data.frame(drugs, score)
kruskal.test(score ~ drugs, data = oscular) 

# Random-effects model
ID<-rep(1:5,2)
y<-c(26.6,11.1,8,20.7,5.8,30.4,15,8.1,16.9,8.4)
logy<-log(y)
library(lme4)

summary(lmer(logy~(1|as.factor(ID))))

# icc: intraclass correlation coefficient
# Correlation bw two replicates from the same subject
# bw yij and yjk where j != k
# One-way random effects ANOVA
# in this case
icc = 0.3260 / (0.3260+0.0287)

