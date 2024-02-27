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

# Zadanie 2
# czesciowa
-10/sqrt(5*26)

z<-rnorm(10000)
Nx<-rnorm(10000)
Ny<-rnorm(10000)
x<- -2*z+Nx
y<- -5*z+Ny

cor(x,y)


