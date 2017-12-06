# Chapter12
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