# ramki danych
iris
class(iris)
attributes(iris)
typeof(iris)
unclass(iris)

# wektory
c(1,2,3)
list(1,2,3)
c(a = 1, b = 4, c = 10)
list(a <- 3:10)
a

##############################################
# cwiczenia z data frame df
data.frame(A = 1:10,
           B = letters[1:10],
           C = c(TRUE, FALSE)) -> df
df

str(df)
df$C
df$A
df$B

# cwiczenia z iris i faktor

attributes(iris$Species)
unclass(iris$Species)

x <- factor(c("a", "b", "a", "b", "a", "a"), levels =c("a","b"))
x
typeof(x)
class(x)
?factor
levels(x)
x[x == "a"]
x[x == "b"]
levels(x) <- c("A", "B")
x
x[x == "b"]
levels(x) <- c("Grupa1", "Grupa2") 
x

factor(c("a", "b", "a", "b", "a", "a", "c"), levels = c("a", "b"))
unclass( factor(c("a", "b", "a", "b", "a", "a", "c"),
         levels = c("a", "b")) )

x
x[7] <- "Grupa 2"
x
levels(x)[3] <- "Grupa 3"
x[7] <- "Grupa 3"
x
x[8] <- "Grupa 3"
x

y <- factor(c("a", "b", "c"),
            levels = c("b", "c", "a"), ordered = TRUE)
y
sort(y)

data.frame(A = 1:10,
           B = letters[1:10],
           C = c(TRUE, FALSE)) -> df
df
str(df)

# Filtrowanie danych / indeksowanie ramek danych
df

# wyboór kolumny/kolumn
df[1] # ramki danych w wyniku
df["B"]
df$B

# wybor wiersza
df[1, ]
df[c(1, 3, 6), ]
df <- df[sample(10), ]
df
df <- df[ ,sample(3)]
df
?sample #zmienia losowo pozycje wierszy
row.names(df)
df[c("1","4","7"),] #wybieranie po etykietach
df[c(1,4,7), ] #wybieranie po elementach

df[c(TRUE, FALSE), c(1, 3)]
df[df$C, ] 

#iris

iris[iris$Species == 'versicolor', ] 
iris[iris$Sepal.Length > mean(iris$Sepal.Length), 'Species']

#wines
wines <- read.csv("winequality-all.csv", comment.char = "#")
