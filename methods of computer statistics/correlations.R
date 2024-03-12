air <- read.table("airpollution.txt", header=T)
mor<-air$Mortality
edu<-air$Education

# test permutacyjny
cor1<-cor(mor,edu)
k<-100000
corrs<-numeric(k)
for(i in 1:k)
{
  morper<-sample(mor)
  corrs[i]<-cor(morper,edu)
}
hist(corrs)
abline(v=cor1,col="red")
(1+sum(abs(corrs)>abs(cor1)))/(1+k) 
# wniosek: p-value bardzo niskie, co oznacza że odrzucamy hipotezę zerową
# więc nasze założenia o nistotności były słuszne

J<-air$JulyTemp
S<-air$S02Pot

# test permutacyjny
cor2<-cor(J,S)
k<-100000
corrs<-numeric(k)
for(i in 1:k)
{
  morper<-sample(J)
  corrs[i]<-cor(morper,S)
}
hist(corrs)
abline(v=cor2,col="red")
(1+sum(abs(corrs)>abs(cor2)))/(1+k) 

# Zadanie 2.2

# czesciowa
-10/sqrt(5*26)
# w sqrt wariancja x

z<-rnorm(1000)
Nx<-rnorm(1000)
Ny<-rnorm(1000)
x<- -2*z+Nx
y<- -5*z+Ny
cor(x,y)

#
install.packages("ppcor")
library(ppcor)
d<-data.frame(x=x, y=y,z=z)

# korelacja częściowa X i Y pod warunkiem Z
pcor(d)

m1 <- lm(x~z)
m2 <- lm(y~z)
# bez wyrazu wolnego:
m1 <- lm(x~z-1)
m2 <- lm(y~z-1)

res1 <- m1$res # res = X - m1$coef*Z
res2 <- m2$res # res = Y - m2$coef*Z

cor(res1,res2)

# Zadanie 2.3
eps <- rnorm(n=101,mean=0,sd=3)
x <- seq(from=0,to=10,by=0.1)
y <- x+eps

# a) korelacja próbkowa:
cor(x,y)
sum((x-mean(x))*(y-mean(y))) / sqrt( sum((x-mean(x))^2) * sum((y-mean(y))^2) )

# b)
m1 <- lm(y~x)
m1$coefficients

# lub:
b1 <- sum((x-mean(x))*(y-mean(y))) / sum((x-mean(x))^2)
b0 <- mean(y)-mean(x)*b1

# wykres:
plot(x,y,main="Wykres rozproszenia")
abline(a=b0, b=b1, col='red', lwd=2)
legend("topleft", legend="MNK", col="red", lty=1, lwd=2)

# na nowych danych sd = 0.5
x <- seq(from=0,to=10,by=0.1)
eps <- rnorm(n=101,mean=0,sd=0.5)
y <- x+eps
cor(x,y)
m1 <- lm(y~x)
m1$coefficients
plot(x,y)
abline(m1, col="red")

# na nowych danych sd = 5
x <- seq(from=0,to=10,by=0.1)
eps <- rnorm(n=101,mean=0,sd=5)
y <- x+eps
cor(x,y)
m1 <- lm(y~x)
m1$coefficients
plot(x,y)
abline(m1, col="red")

# zadanie 4

library(MASS)
hills

# a)
n<-length(hills$time)
m1<-lm(time~dist,data=hills)
m2<-lm(time~climb,data=hills)

par(mfrow=c(1,2))
plot(hills$dist,hills$time)
abline(m1, col="red")
plot(hills$climb,hills$time)
abline(m2, col="red")

cor(hills$dist, hills$time)
cor(hills$climb, hills$time)

# b)
b0<-m1$coef[1]
b1<-m1$coef[2]

y_pred1 <- b0+b1*hills$dist

b0<-m2$coef[1]
b1<-m2$coef[2]

y_pred2 <- b0+b1*hills$dist

summary(m1)$r.squared
summary(m2)$r.squared

sst<-sum((hills$time-mean(hills$time))^2)

sse1<-sum((hills$time-(y_pred))^2)
sse1<-sum((hills$time-predict(m1))^2) # lub
sse2<-sum((hills$time-predict(m2))^2)

ssr1<-sum((y_pred-mean(hills$time))^2)
ssr2<-sum((y_pred2-mean(hills$time))^2)

r2_1 <- ssr1/sst
r2_2 <- ssr2/sst

#c)
predict(m1,data.frame(dist=15))
#lub
sum(m1$coef*c(1,15))

# zadanie 2.5

ans <- read.table("anscombe_quartet.txt",header=T)

m1<-lm(Y1~X1,data=ans)
m1<-lm(Y2~X2,data=ans)
m1<-lm(Y3~X3,data=ans)
m1<-lm(Y4~X4,data=ans)


