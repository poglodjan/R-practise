install.packages(c("nycflights13", "sqldf",
                   "dplyr", "data.table",
                   "compare"))

# Załączamy potrzebne pakiety:
library("nycflights13") # baza danych
# Funkcja sqldf z pakietu sqldf pozwala na wykonanie poleceń języka SQL
# na ramkach danych (za pośrednictwem systemu baz danych RSQLite)
library("sqldf")
# pakiet dplyr wykorzystamy do porównywania wyników:
library("dplyr")
# lub pakiet cmpare
library(compare)

help(package = "nycflights13")
help(package = "sqldf")

####################################################

class(airlines)
airports <- as.data.frame(airports)
airports
flights <- as.data.frame(flights)
flights
planes <- as.data.frame(planes)
weather <- as.data.frame(weather)
airlines <- as.data.frame(airlines)

default.stringsAsFactors()
options(stringsAsFactors = FALSE)
default.stringsAsFactors()

class(planes[ "engine" ])
planes[9]
planes[ , "engine" , drop = FALSE]
class(planes$engine)
planes
class(planes$engine)

## SQL 
# 4.1 analiza ramek danych
?airlines
?flights
?planes
?weather
?airlines
nrow(airlines)
ncol(airlines)
dim(airlines)
airlines
head(airlines) #poczatkowe 6 wierszy
head(airlines, 3) #poczatkowe 3 wiersze
tail(airlines) #koncowe 6 wierszy
tail(airlines, 3)

df <- sqldf("SELECT * FROM planes")  # ... wybierz wszystkie informacje z tabeli samoloty
dim(df)
head(df)

# 4.2 słowa kluczowe (unikatowe)

#SQL:
engines <- sqldf("SELECT engine FROM planes")
head(engines)
unikat_engines <- sqldf("SELECT DISTINCT engine FROM planes")
unikat_engines

#R:
unikat_enginesR <- unique( planes["engine"] )
unikat_enginesR
rownames(unikat_enginesR)

# porównania:
nrow(unikat_engines)
nrow(unikat_enginesR)
all(dim(unikat_engines)==dim(unikat_enginesR))
dplyr::all_equal(unikat_engines, unikat_enginesR)

test_1 <- data.frame(x = 1:10, y = letters[1:10])
test_2 <- data.frame(x1 = seq(1, 10, 1), y = letters[1:10])
all_equal(test_1,test_2)
colnames(test_2)[1] <- "x"
all_equal(test_1,test_2)

# 3 rozwiązanie:
unikat_engines3 <- data.frame( engine = unique( planes$engine ))
unikat_engines3

## 4.3
# wybierz wszystkie unikatowe pary postaci: (typ samolotu, producent)
# SQL: 

df3 <- sqldf("SELECT DISTINCT type, manufacturer
FROM planes")
df3

# R:
df3_R <- unique(planes[ ,c("type","manufacturer")])
planes
df3_R
dplyr::all_equal(df3, df3_R)

# 4.4 Dla każdego typu silnika policz, w ilu samolotach został on zainstalowany.

#SQL
policz <- sqldf("SELECT COUNT(*), engine FROM planes GROUP BY engine")
policz

#R
policz_R <- aggregate(x = planes$engine, by = planes["engine"], FUN=length)
policz_R
# zamienmy kolumny
colnames(policz_R)[2] <- "count"
colnames(policz)[1] <- "count"
colnames(policz)[2] <- "engine"
policz_R <- policz_R[ ,c("count", "engine")]
policz_R
policz
dplyr::all_equal(policz, policz_R)

# R $2
table( planes$engine ) #dostajemy tablice array o el całkowitych
planes$engine #przeksztalcamy na ramke danych

policz_R2 <- as.data.frame( table(planes$engine), stringsAsFactors = FALSE)
colnames(policz_R2) <- c("engine", "count")
policz_R2 <- policz_R2[ ,c("count", "engine")]
policz_R2
dplyr::all_equal(policz_R, policz_R2)

# ROZWIAZANIE 3:
# na około:
groups <- split( planes$engine, planes["engine"] )
counts <- sapply(groups, length) # nazwany wektor numeryczny
# zamieniamy na ramke danych:
df_R4 <- data.frame( count = counts,
                 engine = names(counts) )
df_R4
rownames(df_R4) <- NULL
df_R4

# ROZWIAZANIE 4:
# zob. ?structure
structure(
  as.data.frame(
    table(planes$engine),
    stringsAsFactors = FALSE
  ),
  names=c("engine", "count")
) -> df_R5
df_R5

########################################################
# 4.5
# ... ile jest rodzajow kombinacji silnika i jego typu tzn.
# ... liczba wystąpień wszystkich unikatowych par postaci:
# (typ silnika, typ samolotu)

typy <- sqldf("SELECT COUNT(*) as count, engine, type FROM planes
              GROUP BY engine, type")
typy
# ROZWIAZANIE 1:
# grupujemy pierwszą zmienną (kolumnę) według kombinacji
# par (typ silnika, typ samolotu) i zliczamy (length) ile mamy
# samolotów w każdej takiej grupie

df_typy <- aggregate(planes[1], planes[c("engine","type")], length)
df_typy
names(df_typy)[3] <- "count"
df_typy

# ROZWIAZANIE 2:
# co zwróci funkcja table wywołana dla ramki danych o 2 kolumnach:
table( planes[ ,c("engines", "type")])

df_typy2 <- as.data.frame( table( planes[, c("engine", "type")] ),
                    stringsAsFactors = FALSE )
df_typy2
## pozbywamy się kombinacji dla ktorych jest 0:
df_typy2 <- df_typy2[df_typy2$Freq>0,]
df_typy2
colnames(df_typy2)[3] <- "count"
rownames(df_typy2) <- NULL
df_typy2

# Rozwiazanie 3:
df_typy3 <- as.data.frame( table( planes[c("engine", "type")] ),
                    stringsAsFactors = FALSE )
colnames(df_typy3) <- c("engine", "type", "count")
df_typy3 <- df_typy3[df_typy3$count>0, c("count", "engine", "type")]
df_typy3

# 4.6
# Wyznacz minium (MIN), srednia (AVG) i maksimum (MAX) dla roku (kolumna year)
# w grupach wyznaczonych przez kombinacje kolumn engine i manufacturer

# SQL

df_obl <- sqldf("SELECT MIN(year), AVG(year), MAX(year), engine, manufacturer
                FROM planes GROUP BY engine, manufacturer")
dim(df_obl)

head(df_obl)

# SQL z AS

df_obl2 <- sqldf("SELECT MIN(year) AS year_min, AVG(year) AS year_avg,
                 MAX(year) AS year_max, engine, manufacturer
                 FROM planes 
                 GROUP BY engine, manufacturer")
head(df_obl2)

# R: ROZWIAZANIE:
aggregate(x = planes["year"],
          by = planes[c("engine", "manufacturer")],
          FUN = function(x) c(min=min(x),
                              mean=mean(x),
                              max=max(x))
)

# usuwamy NaN
x <- aggregate(x = planes["year"],
               by = planes[c("engine", "manufacturer")],
               FUN = function(x) c(year_min=min(x, na.rm=TRUE),
                                   year_avg=mean(x, na.rm=TRUE),
                                   year_max=max(x, na.rm=TRUE)))
head(x)
head(x['year'])
head(x[ ,3])
x <- cbind( x[, 3], x[, 1:2] )
head(x[ ,3])
# Zamiana NaN i INF na NA
x[ is.nan(x[, "year_min"]) | is.infinite(x[, "year_min"]), "year_min"] <- NA
x[ is.nan(x[, "year_avg"]) | is.infinite(x[, "year_avg"]), "year_avg"] <- NA
x[ is.nan(x[, "year_max"]) | is.infinite(x[, "year_max"]), "year_max"] <- NA
head(x[ ,3])

##########################################
# 4.6 z where
# Wybierz wszystkie kolumny z tabeli planes ale tylko te rekordy dla ktorych
# wartosci kolumny speed sa niepuste (nie są brakami danych)

data <- sqldf("SELECT * FROM planes WHERE speed IS NOT NULL")
head(data)

# R:
dataR <- planes[ !is.na(planes$speed), ]
rownames(dataR) <- NULL
head(dataR)

# rozw 2
x <- subset(planes, !is.na(speed))
head(x)

########### 4.7
# ... wybierz wartosci kolumny tailnum dla samolotow, ktore maja
# od 150 do 190 miejsc i zostaly wyprodukowane po 2012

data_wyb <- sqldf("SELECT tailnum
             FROM planes
             WHERE seats BETWEEN 150 AND 190 AND year >= 2012")

head(data_wyb)

# R

na.omit(planes[ planes$seats >= 150 & planes$seats <= 190 & planes$year >= 2012,
                "tailnum",
                drop=FALSE]) -> data_wybR
row.names(data_wybR) <- NULL
data_wybR

#problem z NA
sum(is.na(planes$year))
planes[c(NA, rep(FALSE, nrow(planes)-1)), ]

#rozwiazanie 2
subset(planes,
       planes$seats >= 150 & planes$seats <= 190 & planes$year >= 2012,
       tailnum)
?subset

########## 4.7
# ... wybierz wszystkie samoloty, ktore zostaly wyprodukowane przez
# Boeinga lub Airbusa lub Embraera i maja wiecej niz 390 miejsc

x <- planes[ planes$seats > 390 & planes$manufacturer %in% c("BOEING",
                                                             "AIRBUS",
                                                             "EMBRAER"), ]
rownames(x) <- NULL
x

# ... posortuj unikatowe kombinacje kolumn year (rok produkcji) oraz
# seats (liczba mijesc) dla samolotow, ktore zostaly wyprodukowane po 2012
# najpierw po roku (rosnaco -- ASC), a nastepnie po liczbie miejsc
# (malejaco -- DESC)
sqldf("SELECT DISTINCT year, seats
FROM planes
WHERE year >= 2012
ORDER BY year ASC, seats DESC") -> df
print(df)

x <- na.omit( unique( planes[ planes$year >= 2012, c("year", "seats") ] ) )
# sortowanie z wykorzystaniem funkcji ?order
# ale na około:
x <- x[order(x$seats, decreasing = TRUE),]
x <- x[order(x$year),]
rownames(x) <- NULL
x
