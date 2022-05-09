#Zad 5.1

M <- matrix(1:12, ncol=3, byrow=TRUE)
M
M[1][1]
M[1,1]

?cbind # łączy wektory i robi macierz
M[c(1,2),c(1,3)]
cbind(c(1,2),c(1,3)) # pierwsze - ktore wiesze, drugie - ktore kolumny
M
t(cbind(c(1,2),c(1,3))) # macierz transponowana
M[cbind(c(1,2),c(1,3))] # odwołuje do [1][1], [2][3]

t <- c(1,3,1,2,5,5)

onehotenc <- function(t){
  k <- max(t)
  n <- length(t)
  M <- matrix(0, nrow=n, ncol=k)
  M[cbind(1:n,t)] <- 1
}
onehotenc(5)

############ Zad 5.2
M2 <- matrix(1:20, ncol=4, byrow=TRUE)
M2

colSums(M2) # sumy
rowSums(M2)

# zakodowanie
softmax <- function(X){
  stopifnot(is.matrix(X), all( dim(X)>0 ))
  Y <- exp(X)
  return ( Y / rowSums(Y) ) 
}

M_s <- softmax(M2)

# odkodowanie
onehotdec <- function(X){
  Y <- softmax(X)
  apply(Y, 1, function(z){which.max(z)}) #kolumny w których wartości są najbardziej zbliżone do 1
} # jedynka oznacza że chodzimy po wierszach
# potem szukamy wartości maksymalnej

onehotdec(M2)
M_s
M2

# Zadanie 3.3
set.seed(123)
a <- sample(1:100)
range(a)

hyperr <- function(X){
  apply(X, 2, range) #zwraca najmniejsze i najwieksze wartosci
}

M3 <- abs( matrix(rnorm(24), nrow=6) )
hyperr(M3)
M3
plot(M3)

# Zadanie 3.4

odl <- function(X, y){
  Y <- matrix(y, byrow=TRUE, nrow=nrow(X), ncol=length(y))
  Y <- (X-Y)^2
  sqrt(rowSums(Y))
}

M4 <- M3
y <- runif(4)
odl(M4, y) 

# Zadanie 3.5 zmienne losowe

M5 <- cbind(c(0,0.01,0.1,0.2), c(0.01,0.05,0.03,0.1), c(0.1,0.03,0.05,0.01), c(0.2,0.1,0.01,0))
sum(M5) # = 1

niezaleznosc <- function(P){
  wiersze <- rowSums(P)
  kolumny <- colSums(P)
  all()
  return( all(abs(outer(r,c,"*")-P)<1e-9) )
}

x <- sort(runif(4))
y <- sort(runif(4))
niezaleznosc(M5)
