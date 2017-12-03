#10.9_12_gastroenterology
#10.9_McNemar's test
du_treat<-rbind(c(89,5), c(16, 90))
mcnemar.test(du_treat)
du_treat_male<- rbind(c(52,4), c(9, 35))
mcnemar.test(du_treat_male)