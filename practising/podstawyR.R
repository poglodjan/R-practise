x <- 1:15
#W R x[-1] to wszystko po za 1
x[-1]
letters[ c(1,5,10)]
#times powtarzamy cały wektor, each powtarzamy każdy element tyle razy, 
#lenght.out powtarzamy tyle razy żeby mieć zadaną długość
I <- rep(c(TRUE, FALSE, FALSE), times =5)
x[I]
x[c(TRUE, FALSE)] #WYBIERAMY CO DRUGI ELEMENT TRUE-wybieramy, FALSE-usuwamy
x[rep(c(TRUE, FALSE), lenght.out=15)]
y<-c(TRUE, FALSE)
#iris
iris
mean(x[y="setosa"])
x<-iris
min(x)
#wektory atomowe pojedyncze nawiasy [1], wektory uogólnione podwójne nawiasy [[1]]
# '[<-'(t,3,-100) ustawiamy -100 na miejscu 3 w wektorze t
# t[1:3]<-500 modyfikacja 2
# t[1:3] <- c(10,20,30,40) modyfikacja 3
#
# funkcje w R
#zwektoryzowane np.sin, log, exp
#agregujące np. min, max, śr arytm.
#sd() rozproszenie, mean() srednia aryt
#
#Tworzenie funkcji f<-function(x) x**2
#
#Wektory Uogólnione
