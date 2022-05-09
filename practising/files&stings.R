# przetwarzanie napisów

library("stringi")
x <- 'abc abd adf agh'
stri_extract_all_regex(x, 'a.(c|d)')
x <- "kotek kot koteczek"
stri_extract_all_regex(x, "kotek|kot")

# ^ - na początku
# $ - na koncu
x <- c("skrypt.R", "raport.Rmd", "raport.pdf", "raport.R") # - przydatne
stri_extract_all_regex(x, "\\w+\\.R$")

x<- "<p> to jest tekst </p>" # selekcjonowanie w html
stri_extract_all_regex(x, "(?<=<p>).+(?=</p>)")

# przetwarzanie plików
source("Documents/R/asterix.txt")
source(plik)
getwd() # podaj swoją sciezke względną
readLines("Documents/R/asterix.txt")
file.path("Documents/R/bledy.R") # konstrukcja ścieżki

x<- list.files("Documents/R") # lista plików
x
x[1] #pierwszy plik w ścieżce

file.exists("Documents/R/asterix.txt") # sprawdza istnienie
file.create("Documents/R/asterix.html") # tworzy plik

path <- file.path("PDU")
dir.exists(path) # False
dir.create(path)
dir.exists(path) # True
list.files(path, full.names = T)
file.info(path)
#pisanie w pliku
cat("To jest plik tekstowy", file=file.path(path, "pierwszy.txt"))
file.copy("Users/janpoglod/Documents/R/pierwszy.txt" ,path)
files <- list.files(recursive = TRUE, pattern = ".csv")
