#11.1
reticulocytes<-c(3.6, 2.0, 0.3, 0.3, 0.2, 3.0, 0.0, 1.0, 2.2)
lymphocytes <- c(1700, 3078, 1820, 2706, 2086, 2299, 676, 2088, 2013)
anemia_data<-cbind(reticulocytes, lymphocytes)
lm.anemia<- lm(lymphocytes~reticulocytes)
summary(lm.anemia)
#Rsquare<- SSR/SSTO
#Rsquare can be thought of as the proportion 
#of the variance of y that is explained by x 