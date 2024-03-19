pol <- read.table("airpollution.txt", header=T)
pol$NOxPot<-NULL

########################
#
# 4.1
# a) linear model
#
########################

Y <- pol$Mortality
m_all <- lm(Mortality~.,data=pol)
summary(m_all)

########################
#
# b) T
# H0: Bi=0
# H1: Bi!=0
#
########################
m_all$coef

# Beta = (X_t * X)^-1  * X_t * Y
X <- model.matrix(m_all)
Beta <- (solve(t(X) %*% X) %*% t(X) %*% Y)[,1] #non ^-1
n<-nrow(X)
p<-ncol(X)

# T = (bi_hat - 0) / sqrt( sse/(n-p)*(XX)ii )
# T = (coef_i - 0) / sqrt( sse/(n-p)*(XX)ii )
s2<-m_all$residuals%*%m_all$residuals/(n-p)
XX <- t(X) %*% X
solve(XX, tol=10^(-18))
solve( t(X) %*% X )

# or QR distribution:
QR<-qr(X)
R<-qr.R(QR)
solve(t(R) %*% R) # same problem ! 

# solution:
XX_inv<-solve(R) %*% solve(t(R))

## T ##
t_statistics<-m_all$coef/sqrt(s2*diag(XX_inv))

t_statistics[15]
summary(m_all) #estimate - b_hat, std.error - sqrt(s2*diag(XX_inv)), t.value - t_statistics

# ==
summary(m_all)$coef[,3]  
t_statistics
#

# probability: P( |T| >= H )

# ==
summary(m_all)$coef[,4]
2*(1-pt(abs(t_statistics), df=n-p))
# 

summary(m_all)

# X.NonWhite   5.242e+00  8.796e-01   5.960   _____  3.59e-07 *** _____
# Tu bardzo mala wartosc p-value a wiec dane moga byc od tego bardzo zalezne!
# To zalezy czy prawda dobrze odrzucac powoli zmienne o malym p-value


########################
#
# c) F
# H0: B1=...B_p-1 = 0
# H1: isntnieje Bi!=0
#
########################

# F = (SSR/p-1) / (SSE/n-p)

SSR <- sum( (m_all$fitted.values - mean(pol$Mortality))^2 )
SSE <- s2*(n-p)

F_statistics <- SSR*(n-p) / (SSE*(p-1))
F_statistics2 <- summary(m_all)$fstatistic

1-pf(F_statistics,p-1,n-p)
1-pf(F_statistics2[1],F_statistics2[2],F_statistics2[3])

# zad 4.2
n<-100
b0<-0.5
b1<-1

x<-runif(n,-1,1)
eps<-rnorm(n)
y<-b0+b1*x^2+eps
plot(x,y)

m2<-lm(y~x)
summary(m2)

summary(m2)$coef[2,3]^2
summary(m2)$fstatistic

########################
#
# zad 4.3
#
########################

n<-100
B<-100
moc1 <- numeric(B)
moc2 <- numeric(B)
moc3 <- numeric(B)

beta<-c(0.5,1,0.5,0.05)
for(b in 1:B)
{
  X<-matrix(rnorm(n*3), ncol=3)
  X1<-cbind(1,X)
  eps<-rnorm(n)
  y<-X1%*%beta + eps
  m<-lm(y~X)
  pvals<-summary(m)$coef[,4]
  moc1[b] <- ifelse(pvals[2]<0.05,1,0)
  moc2[b] <- ifelse(pvals[3]<0.05,1,0)
  moc3[b] <- ifelse(pvals[4]<0.05,1,0)
}
MOC1<-mean(moc1)
MOC2<-mean(moc2)
MOC3<-mean(moc3)







