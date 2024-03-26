pol <- read.table("airpollution.txt", header=T)
pol$NOxPot <- NULL

########################
#
# task 1
# a) linear model
#
########################

Y <- pol$Mortality
m1 <- lm(Mortality~NOx,data=pol)
summary(m1)
plot(pol$NOx, pol$Mortality)
abline(m1, col='red')

########################
#
# b) linear model with log
#
########################

m2 <- lm(Mortality~log(NOx),data=pol)
summary(m2)
plot(log(pol$NOx), pol$Mortality)
abline(m2, col='red')

########################
#
# c) reyzdua studentyzowane
#
########################

rstandard(m2)
r2<-rstudent(m2)

obs_out<-as.numeric(names(r2[abs(r2)>2]) )

m3<-lm(Mortality~log(NOx), data=pol, subset=-obs_out)
summary(m3)
abline(m3, col="blue")

########################
#
# task 2
# a)
#
########################

cell <- read.table("cellular.txt", header=T)

# linear
plot(cell$Period, cell$Subscribers)
m4 <- lm(Subscribers~Period,data=cell)
summary(m4)
abline(m4, col="red")

# log
plot(cell$Period, log(cell$Subscribers))
m4 <- lm(log(Subscribers)~Period,data=cell)
summary(m4)
abline(m4, col="red")

# sqrt
plot(cell$Period, (cell$Subscribers)^0.5)
m4 <- lm((Subscribers)^0.5~Period,data=cell)
summary(m4)
abline(m4, col="red")

# sqrt 1/4
plot(cell$Period, (cell$Subscribers)^0.25)
m4 <- lm((Subscribers)^0.25~Period,data=cell)
summary(m4)
abline(m4, col="red")


########################
#
# c)
#
########################

library(MASS)
b<-boxcox(Subscribers~Period,data=cell, lambda=seq(0,1,0.01))
b

m4 <- lm((Subscribers)^0.2~Period, data=cell)
plot(cell$Period, cell$Subscribers^0.2)
abline(m4)

summary(m4)



