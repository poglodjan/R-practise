#Autor rozwiązania Jan Pogłód 520575

#### Krok 1 Implementuje funkcje całka i całkaMonteCarlo

calka <- function( f, a, b, n ){ # Implementuje naszą funkcję
  # sprawdzam poprawność danych 
  stopifnot( is.function(f), is.numeric(a), is.numeric(b),
             is.numeric(n), a<b )
  # Tworzę przydatne do obliczeń zmienne na których będę operować
  h <- ( b-a )/n
  fa <- f(a)
  fb <- f(b)
  # Liczę sumę ze wzoru, tworzę wektor i=1,2,3,..,n-1 
  # i drugi wektor x który liczy wartość dla ustalonego i
  i <- 1:(n-1)
  x <- f(a+i*h)
  suma <- sum(x)
  return( h*( fa/2 + suma +fb/2 ) )
}

calkaMonteCarlo <- function( f, a, b, n ){ #z laboratorium
  # Sprawdzam dane
  stopifnot( is.function(f), is.numeric(a), length(a) == 1, 
             is.numeric(b), length(b) == 1,
             a < b, is.numeric(n), n > 0, n == floor(n) )
  # wprowadzam zmienne
  fa = f(a) 
  fb = f(b)
  fmin = min( fa, fb )
  fmax = max( fa, fb )
  # losuje wektory x, y i obliczam ze wzoru
  set.seed(123)
  x <- runif( n, a, b )
  y <- runif( n, fmin, fmax )
  frakcja <- sum( y <= f(x) )/n
  return ( frakcja * (b - a) * ( fmax - fmin ) + ( b - a ) * fmin )
}

#### Krok 2 Testuję funkcje, sprawdzam wyniki i zapisuje w komentarzu

# Przykład 1. dnorm(x) na przedziale (0,3)
f <- function(x) dnorm(x)
# Sprawdzam wyniki, najpierw dla funkcji wbudowanej, potem dla 
# całki metodą trapezów dla n=10,100,1000, na koniec metodą 
# Monte Carlo dla n=10,100,1000
integrate( f,0,3 ) # 0.4986501
calka( f,0,3,10 ) # 0.4985513
calka( f,0,3,100 ) # 0.4986491
calka( f,0,3,1000 ) # 0.4986501
calkaMonteCarlo( f,0,3,10 ) # 0.2500018
calkaMonteCarlo( f,0,3,100 ) # 0.522214
calkaMonteCarlo( f,0,3,1000 ) # 0.4926257

# Przykład 2. własny sinx+cosx dla n=1000 na przedziale (pi,2*pi)
g <- function(x) sin(x)+cos(x)
a <- pi
b <- 2*pi
calka( g,a,b,1000 ) #-1.999998
integrate( g,a,b ) #-2
calkaMonteCarlo( g,a,b,1000 ) #-1.520531

# Przykład 3. własny dla n=10000 na przedziale (0,1)
# Przykład w którym w calka() i integrate() wychodzi identyczna wartość
h <- function(x) x^8+floor(x/9)-exp(3*x)+15
calka( h,0,1,10000 ) #8.749265
integrate( h,0,1 ) #8.749265 
calkaMonteCarlo( h,0,1,10000 ) #8.952327

