###
# Projekt realizowany dla pewnej firmy automatyzujący alokację pracowników
###

# aktualizacja listy pracowników
lista <- read.csv("lista_pracowników.csv", TRUE,";" ,fileEncoding = "UTF-8")
to_new <- read.csv("do_aktual.csv", TRUE,";" ,fileEncoding = "UTF-8")
library('sqldf')
library("stringi")
library("dplyr")

osoby <- lista[1]
today <- stri_list2matrix(stri_split_fixed(Sys.Date(), c('-')))
pom <- today[1]
today[1] <- today[3]
today[3] <- pom
### LISTA
imiona <- lista[2]
nazwiska <- lista[1]
n <- nrow(lista)
w <- nrow(to_new)

for(i in 1:n){
  osoby[i,1] <- sprintf("%s %s",nazwiska[i,1] ,imiona[i,1])
}
lista[ ,1] <- osoby[ ,1] 
lista[ ,2] <- NULL

### to_new
colnames(to_new) <- c("osoba","pion","departament","zespół","stanowisko","etat","czypracuje","rozpoczęciepracy", "zakonczeniepracy")
colnames(lista) <- c("osoba","rozpoczęciepracy", "zakonczeniepracy","etat","stanowisko","zespół","departament","pion","rodzajumowy") 
save_names <- c("Nazwisko.i.imie","Pion","Departament","Zespół","Stanowisko","FTE","Czy.pracuje","Data.rozpoczęcia.pracy", "Data.zakończenia.pracy")

rozpoczeciepracy <- lista[2]
zakonczeniepracy <- lista[3]
etat <- lista[4]
stanowisko <- lista[5]
zespol <- lista[6]
departament <- lista[7]
pion <- lista[8]
rodzajumowy <- lista[9]
miejscepracy <- lista[10]

lista[1] <- osoby
lista[2] <- pion
lista[3] <- departament
lista[4] <- zespol
lista[5] <- stanowisko
lista[6] <- etat
lista[7] <- ""
lista[8] <- rozpoczeciepracy
lista[9] <- zakonczeniepracy

lista[10] <- NULL
colnames(lista) <- c("osoba","pion","departament","zespół","stanowisko","etat","czypracuje","rozpoczęciepracy", "zakonczeniepracy")

for(i in 1:n){
  if (lista[i,9]==""){
    lista[i,7] <- "TAK"
  }
  else{
    kiedy <- stri_list2matrix(stri_split_fixed(lista[i,9], c('.')))
    data_m <- as.numeric(kiedy[2])
    today_m <- as.numeric(today[2])
    data_r <- as.numeric(kiedy[3])
    today_r <- as.numeric(today[3])
    data_d <- as.numeric(kiedy[1])
    today_d <- as.numeric(today[1])
    if(((data_m<today_m) & (data_r==today_r)) | ((data_m<today_m) & (data_r<today_r)) | ((data_d<today_d) & (data_m==today_m) & (data_r==today_r))){
      lista[i,7] <- "NIE"
    }
    else{
      lista[i,7] <- "TAK"
    }
  }
}

df <- sqldf("SELECT * from lista ORDER by osoba")
n <- nrow(df)
rownames(df) <- NULL

merge <- full_join(to_new, df, by="osoba")
m <- nrow(merge)
colnames(df) <- c("osoba","pion","departament","zespół","stanowisko","etat","czypracuje","rozpoczęciepracy", "zakonczeniepracy")

###### AKTUALIZACJA DANYCH
# pion:
for (row in 1:m){
  if(is.na(merge[row,"pion.y"])==TRUE){
    next
  }
  if((merge[row,"pion.y"]=="") & (is.na(merge[row,"pion.y"])==FALSE)){
    merge[row,"pion.x"] <- merge[row,"pion.x"]
  }
  else{
    merge[row,"pion.x"] <- merge[row,"pion.y"]
  }
}
merge[ ,"pion.y"] <- NULL

# Departament
for (row in 1:m){
  if(is.na(merge[row,"departament.y"])==TRUE){
    next
  }
  if((merge[row,"departament.y"]=="") & (is.na(merge[row,"departament.y"])==FALSE)){
    merge[row,"departament.x"] <- merge[row,"departament.x"]
  }
  else{
    merge[row,"departament.x"] <- merge[row,"departament.y"]
  }
}
merge[ ,"departament.y"] <- NULL

# Zespół
for (row in 1:m){
  if(is.na(merge[row,"zespół.y"])==TRUE){
    next
  }
  if((merge[row,"zespół.y"]=="") & (is.na(merge[row,"zespół.y"])==FALSE)){
    merge[row,"zespół.x"] <- merge[row,"zespół.x"]
  }
  else{
    merge[row,"zespół.x"] <- merge[row,"zespół.y"]
  }
}
merge[ ,"zespół.y"] <- NULL

# Stanowisko
for (row in 1:m){
  if(is.na(merge[row,"stanowisko.y"])==TRUE){
    next
  }
  else{
    merge[row,"stanowisko.x"] <- merge[row,"stanowisko.y"]
  }
}
merge[ ,"stanowisko.y"] <- NULL

# Zakończenie pracy
for (row in 1:m){
  if(is.na(merge[row,"zakonczeniepracy.y"])==TRUE){
    next
  }
  else{
    merge[row,"zakonczeniepracy.x"] <- merge[row,"zakonczeniepracy.y"]
  }
}
merge[ ,"zakonczeniepracy.y"] <- NULL

#czypracuje
for (row in 1:m){
  if(is.na(merge[row,"czypracuje.y"])==TRUE){
    next
  }
  else{
    merge[row,"czypracuje.x"] <- merge[row,"czypracuje.y"]
  }
}

for (row in 1:m){
  if(is.na(merge[row,"etat.x"])==TRUE){
    merge[row,"etat.x"] <- merge[row,"etat.y"]
  }
  if(is.na(merge[row,"etat.x"])==FALSE){
    merge[row,"etat.x"] <- merge[row,"etat.x"]
  }
}
merge[,"czypracuje.y"] <- NULL

# rozpoczecie
for (row in 1:m){
  if((is.na(merge[row,"rozpoczęciepracy.x"])==TRUE) & (is.na(merge[row,"rozpoczęciepracy.y"])==FALSE)){
    merge[row,"rozpoczęciepracy.x"] <- merge[row,"rozpoczęciepracy.y"]
  }
}

merge[,"czypracuje.y"] <- NULL
merge[,"etat.y"] <- NULL
merge[,"rozpoczęciepracy.y"] <- NULL

merge <- sqldf("SELECT * FROM merge ORDER BY osoba")
colnames(merge) <- save_names
for(row in 1:m){
  for(col in 1:9)
    if( is.na(merge[row,col])==TRUE){
      merge[row,col]=" "
    }
}
write.csv(merge ,"aktualizacja.csv")

