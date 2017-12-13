#chap13.Design and Analysis for Epidemiologic studies

#1) Measures of effect for categorical data
# p1: prob. of developing disease for exposed individuals
# p2: prob. of developing disease for unexposed individuals
# RD: Risk Differece: p1-p2
# RR: Risk Ratio: p1/p2
# OR: Odds Ratio: (p1/q1)/(p2/q2)

#2) Confounding and Standardization
# confounding variable
# stratification
# standardization

#3) Mental-Haenzel Test
# 두개의 strata에 해당하는 OR 들로부터 overall estimated OR 찾고,
# test of significance for the exposure 할 때 이용

x1<-matrix(c(120,80,111,155),2,2)
x2<-matrix(c(161,130,117,124),2,2)
x<-array(0,c(2,2,2))  #3-dim array
x[,,1]<-x1
x[,,2]<-x2
mantelhaen.test(x)

#4) Multiple Logistic Regression
case<- c(683, 2537)
total<- c(2181, 11284)  #control set = total-case
AGE30<-c(1,0)   # upper than 30: 1
rst<-glm(cbind(case, total-case)~AGE30, family = binomial(link="logit"))   # logistic regression
summary(rst)

predict(rst, type="link")   # the estimator of logit(E에 0,1 대입)
predict(rst, type = "response")   #case/total
OR<-exp(rst$coef)   #same as OR=(ac/bd)

#CI for OR
OR.U<-exp(rst$coef +1.96*summary(rst)$coef[,2])
OR.L<-exp(rst$coef -1.96*summary(rst)$coef[,2])
cbind(OR,OR.L,OR.U)

# Using profile likelihood
exp(cbind(coef(rst),confint(rst)))

#5) Dose-response study_logistic regression
load("Data/RData_logistic.RData")
beetles[1:10,] # ungrouped format
beetles2 # grouped format

## ungrouped data
fit1<-glm(y~x, data=beetles, family=binomial(link=logit))
summary(fit1)

## grouped data 
fit2<-glm(cbind(dead, n-dead)~logdose, data=beetles2, family=binomial(link=logit))
summary(fit2)

