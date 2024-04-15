phila <- read.table("phila.txt", header=T)
phila

#5.2
########################
#
# b) preprocessing
#
########################

Y <- phila$HousePrice
m1 <- lm(HousePrice~CrimeRate, data=phila)
summary(m1)
plot(phila$CrimeRate, phila$HousePrice)
abline(m1, col='red')

########################
# NANs
########################
nans <- which(is.na(phila$CrimeRate))
phila_fix <- phila[-nans,]
plot(phila_fix$CrimeRate, phila_fix$HousePrice)
m2 <- lm(HousePrice~CrimeRate, data=phila_fix)
########################
# outliers + wplywowosc
########################
outlier <- which(phila_fix$CrimeRate>300)
whi <- which(abs(rstudent(m2))>2) # - otliery
whi
names(whi) <- NULL

X<-model.matrix(m2)
p<-ncol(X)
n<-nrow(X)
hatvalues(m2)
which( hatvalues(m2)>(2*p/n) ) # - obserwacje wplywowe

########################
#
# b) fixed linear model
#
########################

m3<-lm(HousePrice~CrimeRate, data=phila_fix, subset=-63)
abline(m2,col='blue')
abline(m3,col='red')
summary(m2)
summary(m3)

########################
#
# c) cooks distance - szukanie obserwacji wplywowych
#
########################
cook<-cooks.distance(m2)
plot(m2,which=4)
#4 / (n-p-1)

# liczenie cooka:
cook2<- rstandard(m2)^2*hatvalues(m2) / (p*(1-hatvalues(m2)))
cook2[63]
cook[63]

################################################################################
#5.4
sav <- read.table("savings.txt", header=T)
m1 <- lm(Savings~. -Country, data=sav)

#outliers
rs <- rstudent(m1)
which(abs(rs)>2)

hat<-hatvalues(m1)
X<-model.matrix(m1)

which(hat>2*ncol(X)/nrow(X))
cook<-cooks.distance(m1)
plot(m1, which=4)
cook[cook==max(cook)]

m2<-lm(Savings~. -Country, data=sav, subset=-49)
summary(m1)
summary(m2)

# b  - częsciowe wykresy regresji dla mniennej dpi i dppi
install.packages("fawaway") 
library(faraway)
par(mfrow=c(2,1))
prplot(m2,3)
# cz. residua
plot(m2$coefficients[4]*sav$dpi[-49]+residuals(m2)~sav$dpi[-49])
mm23<-lm(m2$coefficients[4]*sav$dpi[-49]+residuals(m2)~sav$dpi[-49])
abline(mm23)
prplot(m2,4)
plot(m2$coefficients[5]*sav$ddpi[-49]+residuals(m2)~sav$ddpi[-49])
mm24<-lm(m2$coefficients[5]*sav$ddpi[-49]+residuals(m2)~sav$ddpi[-49])
abline(mm24)

# cz. regresji
# dpi
m21<-lm(dpi~.-Savings-Country,data=sav,subset = -49)
m22<-lm(Savings~.-dpi-Country,data=sav,subset = -49)
plot(m22$residuals~m21$residuals) # na jednej osi residua jednego modelu, na drugim drugiego modelu
abline(lm(m22$residuals~m21$residuals))
# ddpi
m23<-lm(ddpi~.-Savings-Country,data=sav,subset = -49)
m24<-lm(Savings~.-ddpi-Country,data=sav,subset = -49)
plot(m24$residuals~m23$residuals)
abline(lm(m24$residuals~m23$residuals))

# c)
summary(m21)
summary(m23)
cor(sav$Pop15[-49],sav$Pop75[-49])
summary(m2,cor = T)

# d)
par(mfrow=c(1,1))
prplot(m2,1)
s2<-sav[-49]
m3<-lm(Savings~.-Country,data=s2,subset = Pop15<35)
m4<-lm(Savings~.-Country,data=s2,subset = Pop15>35)

################################################################################
#Zad 5.5
 # nie ma jednakowej wariancji na wszystkich wariancjach - homoskedantyczność
n<-90
x1<-runif(n/3,0,10)
x2<-runif(n/3,10,20)
x3<-runif(n/3,20,30)

x<-c(x1,x2,x3)

eps1<-rnorm(n/3,0,1)
eps2<-rnorm(n/3,0,3)
eps3<-rnorm(n/3,0,5)

eps<-c(eps1,eps2,eps3)

y<-x+eps
x<-c(x,5)
y<-c(y,10)
plot(y~x)
m1<-lm(y~x)
plot(1:(n+1),rstudent(m1))

rez<-residuals(m1)
plot(1:(n+1),rez)

# przewidywanie wariancji
weights<-1/(lm(abs(rez)~m1$fitted.values)$fitted.values^2)
plot(abs(rez)~m1$fitted.values)
abline(lm(abs(rez)~m1$fitted.values))

m2<-lm(y~x,weights = weights)
plot(1:(n+1),rstudent(m2))

abs(rstudent(m1))>2
abs(rstudent(m2))>2



q<-rnorm(10)
qqnorm(q)

