library(dplyr)

data <- read.csv("daneSoc.csv", sep=";", header=TRUE)
View(data)

head(data)
typeof(data)
nrow(data)
ncol(data)
class(data$wyksztalcenie)
sapply(data,class)
lapply(data,summary)
summary(as.factor(data$wyksztalcenie))

# task 1 _______

# b)
table(data$wyksztalcenie)
table(data$praca)

# c)
mezczyzni_srednie = data %>% filter(plec == "mezczyzna", wyksztalcenie == "srednie")
summary(mezczyzni_srednie$cisnienie.skurczowe)
sd(mezczyzni_srednie$cisnienie.skurczowe)
# or 
data[data$plec == "mezczyzna"&data$wyksztalcenie == "srednie",]

# d)
pracujacy = data %>% filter(praca == "uczen lub pracuje")
boxplot(pracujacy$cisnienie.skurczowe)
nie_pracuje = data %>% filter(praca != "uczen lub pracuje")
boxplot(nie_pracuje$cisnienie.skurczowe,pracujacy$cisnienie.skurczowe, col = c("lightblue", "lightcoral"), names = c("Pracuje", "Nie pracuje"), ylab = "Cisnienie skurczowe")

# e)
pacjenci <- data %>% filter(wyksztalcenie == "srednie" & cisnienie.skurczowe > 140 & cisnienie.skurczowe < 150)
pacjenci

#f)
max_pres = max(data$cisnienie.skurczowe)
pacjenci <- data %>% filter(cisnienie.skurczowe == max_pres)
pacjenci

quantile(data$cisnienie.skurczowe,0.8)

# task 2 _______

?rnorm
par(mfrow = c(1, 3))
a<-rnorm(500, mean=0,sd=1) #N(0,1)
qqnorm(a)
qqline(a)
b <- rgamma(500,2,2) #gamma(2,2)
qqnorm(b)
qqline(b)
c <- rcauchy(500)
qqnorm(c)
qqline(c)

# task 3 _______

par(mfrow = c(1, 1))
skor <- read.table("skorelowana_probka.txt", header=TRUE)
x <- skor$x
y <- skor$y
plot(x,y)

cor(x,y)
sum((x-mean(x))*(y-mean(y))) / sqrt(sum((x-mean(x))^2) * sum((y-mean(y))^2))

2*(1-pnorm(sqrt(length(x))*cor(x,y))) # therefore we have grounds to reject the null hypothesis



