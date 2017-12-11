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