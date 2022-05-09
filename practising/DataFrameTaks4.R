# airports <- read.csv("airports.csv", comment.char="#") po co?
airports

class(airports)
class(airports[1])
class(airports[,1, drop=FALSE])
class(airports[,1, drop=TRUE])

class(airports[[1]])

class(airports[-1])

dim(airports)[1]
nrow(airports)
ncol(airports)

# Zadanie 3.7 (MG).
# Wykonaj nastepujace polecenia.
# (a) Wybierz 100 losowych wierszy z ramki danych airports.
# (b) Wybierz 5% wierszy w spos´ob losowy.
# (c) Wybierz 100 pierwszy wierszy.
# (d) Wybierz 100 ostatnich wierszy.

# Zadanie 3.7 a
airports <- read.csv("airports.csv", comment.char="#")

set.seed(123)

a <- sample(nrow(airports),100)
a <- sort(sample(nrow(airports),100))
df_a <- airports[sample(nrow(airports),100),]
df_a1 <- airports[sort(sample(nrow(airports),100)),]

library(magrittr)
# install.packages("dplyr")
library("dplyr")
?sample_n

df_a2 <- sample_n(airports, 100)

df_a
df_a1
df_a2
# Zadanie 3.7 b

df_b <- airports[sample(nrow(airports),round(0.05*nrow(airports))),]
df_b
?sample_frac

df_b1 <- airports %>% sample_frac(0.05)

# Zadanie 3.7 c
df_c <- head(airports, 100)

# Zadanie 3.7 d
df_d <- tail(airports, 100)


# Zadanie 3.8 (MG).
# Na podstawie ramki danych airports stwórz ramkę airports_US zawierającą, tylko iso_country US.
# Następnie podziel ją w sposób losowy na dwie ramki: airports_train (80% wszystkich wierszy) 
# oraz airports_test (pozostałe 20%).

df_airports_US <- airports[which(airports$iso_country == 'PL'),]
rownames(df_airports_US) <- NULL 

sample_train <- sort(sample(nrow(df_airports_US),
                  size=round(0.80*nrow(df_airports_US)), replace=FALSE))

airports_train <- df_airports_US[sample_train,]
airports_test <- df_airports_US[-sample_train,]

# Zadanie 3.9 (MG).
# Do ramki danych vehicles z pakietu fueleconomy (fueleconomy::vehicles) dodaj
# nową kolumnę z_cty oraz z_hwy, takie, że są to ustandaryzowane (z-scores) zmienne cty
# (city-) and hwy (highway-fuel economy, in mpg) w grupach wyznaczonych przez kolumne
# class. Innymi słowy, standaryzujemy (średnia = 0, odchylenie standardowe = 1) zmienne
# w każdej podgrupie. Na początku jednak zamień jednostki z mile-per-galon na 1l/100km.

install.packages("fueleconomy")
library("fueleconomy")

a <- vehicles[1:100,]

min(a[,"hwy"])
max(a[,"hwy"])
mean(a$hwy)

a$z_hwy <- (a$hwy - mean(a$hwy))/sd(a$hwy)
a$z_cty <- (a$cty - mean(a$cty))/sd(a$cty)

z_score <- function(col){
  a <- (col - mean(col))/sd(col)
  a
}

a$new_hwy <- z_score(a$hwy)
a$new_cty <- z_score(a$cty)
a$new_cty <- z_score(a$z_cty)

# Zadanie 3.10 ("Rozwijanie" macierzy).
# Napisz funkcję rozwin(), która przekształca daną macierz rozmiaru n×m 
# (niekoniecznie liczbową) z ustawionym atrybutem dimnames na ramkę danych zawierającą nm obser-
#   wacji i trzy kolumny o nazwach zadanych przy uzyciu odpowiedniego argumentu funkcji.
# Wartości z macierzy mają znajdować się w pierwszej kolumnie, a w kolejnych dwóch {
#   kombinacje nazw wierszy i kolumn odpowiadające podanym poziomom czynnika.
#   Na przykład obiekt WorldPhones (wbudowany) zawiera dane o liczbie telefonów
#   (w tysiacach) w roznych regionach swiata w wybranych latach. Wynikiem wywolania
#   rozwin(WorldPhones, c("ile", "gdzie", "kiedy")) moze byc:
#     ile gdzie kiedy
#   ...
#   2 60423 N.Amer 1956
#   3 64721 N.Amer 1957
#   ...
#   9 29990 Europe 1956
#   10 32510 Europe 1957
#   ...

WP <- WorldPhones
class(WorldPhones)

rozwin <- function(macierz, wektor){
  stopifnot(is.matrix(macierz))
  stopifnot(is.vector(wektor), length(wektor)==3)
  
  # konwersja macierzy na dataframe
  df <- as.data.frame(as.table(macierz))
  # zamiana kolejności kolumn
  df <- df[,3:1]
  colnames(df) <- wektor

  return(df)
}

rozwin2 <- function(macierz, wektor){
  stopifnot(is.matrix(macierz))
  stopifnot(is.vector(wektor), length(wektor)==3)
  
  n <- dim(macierz)[1]
  m <- dim(macierz)[2]
  
  df <- data.frame(matrix(ncol=3, nrow=n*m))
  colnames(df) <- wektor
  kiedy <- unlist(dimnames(macierz)[1])
  gdzie <- unlist(dimnames(macierz)[2])
  # z wykorzystaniem pętli
  for (i in 1:m)
    for (j in 1:n)
    {
      df[n * (i-1) + j, wektor[1]] <- macierz[j, i]
      df[n * (i-1) + j, wektor[2]] <- gdzie[i]
      df[n * (i-1) + j, wektor[3]] <- kiedy[j]
    }
  return(df)
}

rozwin3 <- function(x){

  stopifnot(is.numeric(x), is.matrix(x))
  stopifnot( length(dimnames(x)) == 2 )

  data.frame(
    Rows = rep(rownames(x), ncol(x)),
    Cols = rep(colnames(x), each = nrow(x)),
    Values = as.numeric(x)
  )
}


macierz <- WP[1:7,1:5]
macierz <- WorldPhones
wektor <- c("ile", "gdzie", "kiedy")

df <- rozwin(macierz, wektor)
df2 <- rozwin2(macierz, wektor)
df3 <- rozwin3(macierz, wektor)

# Zadanie 3.11 (MG; "Odwijanie" macierzy)

odwijanie <- function(df){

  stopifnot(is.data.frame(df))

  cn <- colnames(df)
  
  gdzie <- unique(df$gdzie)
  kiedy <- unique(df$kiedy)

  # gdzie <- n_distinct(df$gdzie)
  
  m <- matrix(ncol=length(gdzie), nrow=length(kiedy), dimnames = list(kiedy, gdzie))
  # rownames(m) <- kiedy
  # colnames(m) <- gdzie
  
  # dimnames(m) = list(
  #   kiedy, # row names 
  #   gdzie) # column names 
  # z wykorzystaniem pętli
  for (i in 1:nrow(df))
    {
      rzad <- df[i, cn[3]]
      kol <- df[i, cn[2]]
      m[rzad, kol] <- df[i, cn[1]]
    }
  
  return(m)
}

install.packages("tidyverse",dependencies = TRUE)
library(tidyverse)

odwijanie2 <- function(df){
  
  cn <- colnames(df)
  # wykorzystanie pakiety oraz pivot dataframe
  pivot_df <- pivot_wider(df, id_cols = cn[3], names_from = cn[2], values_from = cn[1])
  v_index <- pivot_df[,1, drop=TRUE]
  pivot_df <- pivot_df[-1]
  rownames(pivot_df) <- v_index
  
  m <- as.matrix(pivot_df)

  return(m)
}

odwijanie(df)
odwijanie2(df)
