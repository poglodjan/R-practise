est <- read.table("realest.txt", header=T)
View(est)

m<-length(est$Price)
m1<-lm(Price~Bedroom,data=est)
m2<-lm(Price~Space,data=est)
m3<-lm(Price~Room,data=est)
m4<-lm(Price~Lot,data=est)
m5<-lm(Price~Tax,data=est)
m6<-lm(Price~Bathroom,data=est)
m7<-lm(Price~Garage,data=est)
m8<-lm(Price~Condition,data=est)

par(mfrow=c(3,3))
plot(est$Bedroom,est$Price)
abline(m1, col="red")
plot(est$Space,est$Price)
abline(m2, col="red")
plot(est$Room,est$Price)
abline(m3, col="red")
plot(est$Lot,est$Price)
abline(m4, col="red")
plot(est$Tax,est$Price)
abline(m5, col="red")
plot(est$Bathroom,est$Price)
abline(m6, col="red")
plot(est$Garage,est$Price)
abline(m7, col="red")
plot(est$Condition,est$Price)
abline(m8, col="red")

# self implement model
# Beta = (X_t * X)^-1  * X_t * Y

Y <- est$Price

###########
#
# a) macierz eksperymentu
#
###########
m_all <- lm(Price~.,data=est)
X <- model.matrix(m_all)
beta_hat <- (solve( t(X) %*% X ) %*% t(X) %*% Y)[,1] # this beta from m1&coef
m_all$coef

###########
#
# b) SST,SSR,SSE
#
###########
SST <- sum((Y-mean(Y))^2)
SSR <- sum((m_all$fitted.values-mean(Y))^2)
SSE <- sum((Y-m_all$fitted.values)^2)
(SSE+SSR)==SST

# r2
r2<-SSR/SST
r2==summary(m_all)$r.squared

###########
#
# c) Jaki wpływ na cenę ma zwiększenie liczby sypialni o 1
#
###########
beta_hat
m_bedroom<-lm(Price~Bedroom,data=est)
m_bedroom$coef

###########
#
# d) predict
#
###########
new_obs <- data.frame(Bedroom=3, Space=1500, Room=8,Lot=40,Bathroom=2,Garage=1,Tax=1000,Condition=0)
predict(m_all,newdata=new_obs)

# lub:
(c(1,3,1500,8,40,1000,2,1,0)%*%beta_hat)[1,1]

###########
#
# d) var
#
###########
# sigm^2 = e_T*e/(n-p)
# sigm^2 = SSE/(n-p)

n<-nrow(X)
p<-ncol(X)
SSE/(n-p)

###########
#
# Zadanie 3 a)
#
###########
B0<-2
B1<-0.5
B2<-1
B3<-7
b<-c(B0,B1,B2,B3)
eps<-rnorm(n=100,mean=0,sd=10)
p<-length(b)
x<-matrix(rnorm(n*(p-1)),ncol=p-1)
X1<-cbind(1,x)
y<-X1%*%b+eps
m1<-lm(y-x)
summary(m1)$sigma^2

###########
#
# b)
#
###########
B0<-2
B1<-0.5
B2<-1
B3<-7
b<-c(B0,B1,B2,B3)
eps<-rnorm(n=100,mean=0,sd=10)
p<-length(b)
k<-1000
sigmas2<-numeric(k)

for(i in 1:k){
  x<-matrix(rnorm(n*(p-1)),ncol=p-1)
  X1<-cbind(1,x)
  y<-X1%*%b+eps
  m1<-lm(y-x)
  sigmas2[i]<-summary(m1)$sigma^2
}
mean(sigmas2)

