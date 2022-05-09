#2.2
calkaMonteCarlo <- function(f, a, b, n = 1000){
 #Sprawdzanie
  stopifnot(is.function(f))
  stopifnot(is.numeric(a), length(a) == 1, 
            is.numeric(b), length(b) == 1,
            a < b)
  stopifnot(is.numeric(n), n > 0, n == floor(n))
  
  fa = f(a) 
  fb = f(b)
  stopifnot(fa >= 0, fb >= 0) 
  fMIN = min(fa, fb)
  fMAX = max(fa, fb)
  
  x <- runif(n, min = a, max = b)
  y <- runif(n, fMIN, fMAX)

  ( sum(y <= f(x))/n ) * (b - a) * (fMAX - fMIN) + (b - a) * fMIN
  
}

set.seed(123)
f <- function(x) sin(x)
a <- 0
b <- 1
n <- 10000
calkaMonteCarlo(f,a,b,n)

#2.5

x <- c(rep(1:5),1,7)
y <- c(1:4, 9, 1, 7)
intersect(x,y) #przeciecie x i y

intersect2 <- function(x,y){
  stopifnot(is.vector(x), is.vector(y), is.numeric(x), is.numeric(y))
  l_max <- max(x,y)
  return (which(tabulate(x, l_max) & tabulate(y, l_max)))
}

intersect2(x,y)

#2.7

merge_string_list <- function(x, sep="") {
  paste(unlist(x), collapse = sep)
}

k <- list(c("a","b","c"))
merge_string_list(k)
k

#2.8

posortowanePunkty <- function(n, m, z){
  stopifnot(is.numeric(n), length(n) == 1) # wymiar przestrzeni
  stopifnot(is.numeric(m), length(m) == 1) # liczba punktow
  stopifnot(is.numeric(z), length(z) == n*m) 
  
  groups <- rep(1:m, n) 
  points <- split(z, groups)
  odl <- sapply(points, function(x) sqrt(sum(x^2)) )
  unsplit(points[order(odl)], groups)
}

z <- 1:6
z <- c(2, 1, 3, 5, 4, 6)
n <- 2 # wymiar
m <- 3 # liczba punktow
posortowanePunkty(n, m, z)

#2.9
approxinvert <- function(f,a,b,k=100){
  
}
  
