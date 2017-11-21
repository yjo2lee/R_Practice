#Q.3
#(1)
car.sales <- c(27, 27, 30, 28, 37, 25, 22, 31, 23, 32, 28, 34
)
names(car.sales)<-month.name
#(2)
sum(sort(car.sales)[0:3])
#(3)
car.sales[car.sales[] > mean(car.sales)]
#(4)
names(diff(car.sales, 1)[diff(car.sales,1)[]==max(diff(car.sales,1))])
#(5)
mod.sales <- car.sales %% 10
c(0:9)[!is.element(c(1:10), mod.sales)]
c(0:9)[is.na(match(c(0:9), mod.sales))]
#(6)
car.sales[c(5,7)] = NA
#(7)
car.sales[c(5,7)] = mean(car.sales, na.rm=TRUE)