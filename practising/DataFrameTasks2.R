library("nycflights13")
library("sqldf")
library("dplyr")
library(compare)

class(airlines)
View(airlines)

#konwersja
airports <- as.data.frame(airports)
airlines <- as.data.frame(airlines)
flights <- as.data.frame(flights)
weather <- as.data.frame(weather)
planes <- as.data.frame(planes)

engine <- planes$engine
engine
engine[33]
View(engine)

t <- planes[ , 3]
View(t)

head(airlines)
df <- sqldf("SELECT * FROM planes")
df
df <- sqldf("SELECT engine FROM planes")
df

# rodzaje silników:
rodzaje <- sqldf("SELECT DISTINCT engine FROM planes") 
rodzaje_R <- unique( planes["engine"] )
rownames(rodzaje_R) <- NULL
rodzaje_R
rodzaje
View(rodzaje_R)
dplyr::all_equal(rodzaje,rodzaje_R)

silnik1 <- rodzaje_R[1,1] #jeden napis
silnik2 <- rodzaje_R[2,1]
silnik1
silnik2
class(silnik1)

to_list <- c(rodzaje_R)
to_list
View(to_list)

# policzenie silników:
policz <- sqldf("SELECT COUNT(*), engine FROM planes GROUP BY engine")
ilosc <- c(policz[1])
rodzaj <- c(policz[2])

# Wybierz tylko unikatowe kombinacje wartosci kolumn type i manufacturer

types <- sqldf("SELECT DISTINCT type, manufacturer FROM planes")
View(types)
type <- types[1]
manufacturer <- types[2]
type[5,1]

types_R <- unique( planes[ , c("type", "manufacturer")])
rownames(types_R) <- NULL
dplyr::all_equal(types, types_R)

# ilosc silnikow w poszczegolnych typach samolotów
ilosc_silnikow <- sqldf("SELECT COUNT(*) AS count, engine FROM planes GROUP BY engine")
colnames(ilosc_silnikow) <- "count"
silnik5 <- ilosc_silnikow[5,2]
ilosc5 <- ilosc_silnikow[5,1]
wynik5 <- c(ilosc5, silnik5)
wynik5

ilosc_silnikow_R <- aggregate(x = planes$engine, by = planes["engine"], FUN=length)
colnames(ilosc_silnikow_R)[2] <- "count"
ilosc_silnikow_R <- ilosc_silnikow_R[ ,c("count", "engine")] #-zamienia kolumny
ilosc_silnikow_R

planes$engine
table <- table( planes$engine )
x <- as.data.frame( table , stringsAsFactors = FALSE)
colnames(x) <- c("engine", "count")
x <- x[ ,c("count", "engine")]
x
dplyr::all_equal(x, ilosc_silnikow)

groups <- split(planes$engine, planes["engine"])
counts <- sapply(groups,length)
x2 <- data.frame(count = counts, engine = names(counts))
x2  
