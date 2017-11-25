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




