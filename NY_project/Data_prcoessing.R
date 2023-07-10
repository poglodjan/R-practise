# Analiza wypożyczeń rowerów citibike w Nowym Jorku w okresie 5 lat 

df_lipiec18 <- read.csv("201807-citibike-tripdata.csv")
df_pazdziernik18 <- read.csv("201810-citibike-tripdata.csv")
df_marzec18 <- read.csv("201803-citibike-tripdata.csv")
df_data <- rbind(df_marzec18, df_lipiec18, df_pazdziernik18)
library("dplyr")
library("tidyverse")

# ----------- "Wpływ pór roku na ruch rowerowy na mieście i
#jak dzięki temu możemy przewidzieć popularne trasy na przyszłość?" -----------

oblicz_dystans <- function(i,dlugosc_konca,dlugosc_startu,szerokosc_konca,szerokosc_startu){
  # funkcja liczy odlegosc euklidesową miedzy stacjami które pokonał rowerzysta
  km_szerokosc <- ( szerokosc_startu[i,1]-szerokosc_konca[i,1] )*111.120 
  km_dlugosc <- ( dlugosc_startu[i,1]-dlugosc_konca[i,1] )*111.120*cos(szerokosc_startu[i,1]-szerokosc_konca[i,1] )
  odlegosc_stacji <- sqrt(km_dlugosc**2+km_szerokosc**2)
  return(odlegosc_stacji)
}

generuj_dystanse <- function(df){
  dlugosc_startu <- df %>% select(c("start.station.longitude"))
  szerokosc_startu <- df %>% select(c("start.station.latitude"))
  dlugosc_konca <- df %>% select(c("end.station.longitude"))
  szerokosc_konca <- df %>% select(c("end.station.latitude"))
  badanie_dystansow <- df %>% select(c("tripduration",
                                       "start.station.id","end.station.id",
                                       "start.station.name","end.station.name",
                                       "birth.year","gender")) %>% 
    mutate(Dystans="")
  n <- nrow(badanie_dystansow)
  m <- ncol(badanie_dystansow)
  przechowaj_dystanse <- matrix(1:n)
  for( i in 1:n ){
    dystans <- oblicz_dystans(i,dlugosc_konca,dlugosc_startu,szerokosc_konca,szerokosc_startu)
    przechowaj_dystanse[i] <- dystans
  }
  sredni_dystans <- mean(przechowaj_dystanse)
  # najpopularniejsze dla wszystkich 
  badanie_dystansow <- badanie_dystansow %>% 
    mutate( Dystans = przechowaj_dystanse)
  return(badanie_dystansow)
}

generuj_wykres_najdluzszych <-  function(df){
  mlodzi <- df %>% filter(birth.year>=1993) # do 25 lat
  sredni <- df %>% filter(birth.year>=1958&birth.year<1993) # od 25 do 60 lat
  starsi <- df %>% filter(birth.year<1953) # od 60 lat 
  liczba_starszych <- nrow(starsi) 
  liczb_srednich <- nrow(sredni) 
  liczba_mlodych <- nrow(mlodzi) 
  # Badanie długich tras
  dlugie_mlodzi <- nrow( mlodzi %>% filter(Dystans>8) )
  dlugie_sredni <- nrow( sredni %>% filter(Dystans>8) )
  dlugie_starsi <- nrow( starsi %>% filter(Dystans>8) )
  procent_mlodzi <- (dlugie_mlodzi / liczba_mlodych*100) 
  procent_sredni <- (dlugie_sredni / liczb_srednich*100)
  procent_starzy <- (dlugie_starsi / liczba_starszych*100)
  wykres <- barplot(names.arg=c("mlodzi","pracujący","emeryci") 
                    ,c(procent_mlodzi,procent_sredni,procent_starzy),
                    col=rainbow(9),
                    xlab="Najdluzsze trasy podział procentowy")
  return(wykres)
}

genreruj_wykres_zerowy <- function(df){
  mlodzi <- df %>% filter(birth.year>=1993) # do 25 lat
  sredni <- df %>% filter(birth.year>=1958&birth.year<1993) # od 25 do 60 lat
  starsi <- df %>% filter(birth.year<1953) # od 60 lat 
  liczba_starszych <- nrow(starsi) 
  liczb_srednich <- nrow(sredni) 
  liczba_mlodych <- nrow(mlodzi) 
  zerowe_mlodzi <- nrow( mlodzi %>% filter(Dystans==0) )
  zerowe_sredni <- nrow( sredni %>% filter(Dystans==0) )
  zerowe_starsi <- nrow( starsi %>% filter(Dystans==0) )
  procent_0_mlodzi <- (zerowe_mlodzi / liczba_mlodych*100) 
  procent_0_sredni <- (zerowe_sredni / liczb_srednich*100) 
  procent_0_starzy <- (zerowe_starsi / liczba_starszych*100) 
  wykres <- barplot(names.arg=c("mlodzi","pracujący","emeryci") 
                    ,c(procent_0_mlodzi,procent_0_sredni,procent_0_starzy), 
                    col=rainbow(9), 
                    xlab="Zwrot w tej samej stacji co wypożyczenie")
  return(wykres)
}

genreruj_wykres_liczbaosob <- function(df){
  mlodzi <- df %>% filter(birth.year>=1993) # do 25 lat
  sredni <- df %>% filter(birth.year>=1958&birth.year<1993) # od 25 do 60 lat
  starsi <- df %>% filter(birth.year<1953) # od 60 lat 
  liczba_starszych <- nrow(starsi) 
  liczb_srednich <- nrow(sredni) 
  liczba_mlodych <- nrow(mlodzi) 
  wykres <- barplot(names.arg=c("mlodzi","pracujący","emeryci") 
                    ,c(liczba_mlodych,liczb_srednich,liczba_starszych), 
                    col=rainbow(9), 
                    xlab="Zwrot w tej samej stacji co wypożyczenie")
  return(wykres)
}

najpopularniejsze_trasy <- function(df){
  najpopularniejsze <- head( df %>%
                               filter(start.station.name!="NULL") %>%
                               filter(birth.year<1953) %>%
                               count(start.station.name,end.station.name, sort=TRUE),5)
  return(najpopularniejsze)
}

najdluzsze_trasy <- function(df){
  najdluzsze <- head( df %>% filter(Dystans>15) %>%
                        filter(start.station.name!="NULL") %>%
                        count(start.station.name,end.station.name, sort=TRUE),5)
  return(najdluzsze)
}

# -----------------)

edit_siepien <- generuj_dystanse(sierpien)
edit_luty <- generuj_dystanse(luty)
edit_maj <- generuj_dystanse(maj)
edit_pazdziernik <- generuj_dystanse(pazdziernik)

## generowanie wyników

generuj_wykres_najdluzszych(edit_luty)
genreruj_wykres_zerowy(edit_luty)
najdluzsze_trasy(edit_luty)
najpopularniejsze_trasy(edit_luty)

# ------------ Zapytanie "Jak dni tygodnia wpływają na ruch rowerowy?" ------------

# Badanie ilosci wypozyczen dla calego badanego okresu w obrebie poszczegolnych dni tygodnia
overall <- function(df_data){
  df_data$data_jazdy <- as.Date(df_data$starttime)
  df_data$dzien_tygodnia <- weekdays(df_data$data_jazdy)
  
  mon <- df_data %>% filter(dzien_tygodnia == "poniedzia?ek",  usertype == "Subscriber") %>% nrow() #562570
  tue <- df_data %>% filter(dzien_tygodnia == "wtorek",  usertype == "Subscriber") %>% nrow() #611408
  wed <- df_data %>% filter(dzien_tygodnia == "?roda",  usertype == "Subscriber") %>% nrow() #534708
  thu <- df_data %>% filter(dzien_tygodnia == "czwartek",  usertype == "Subscriber") %>%nrow() #474283
  fri <- df_data %>% filter(dzien_tygodnia == "pi?tek",  usertype == "Subscriber") %>% nrow() #454405
  sat <- df_data %>% filter(dzien_tygodnia == "sobota",  usertype == "Subscriber") %>% nrow() #318018
  sun <- df_data %>% filter(dzien_tygodnia == "niedziela",  usertype == "Subscriber") %>% nrow() #359610
  
  mon2 <- df_data %>% filter(dzien_tygodnia == "poniedzia?ek", usertype == "Customer" ) %>% nrow() #59761
  tue2 <- df_data %>% filter(dzien_tygodnia == "wtorek", usertype == "Customer" ) %>% nrow() #54951
  wed2 <- df_data %>% filter(dzien_tygodnia == "?roda", usertype == "Customer" ) %>% nrow() #56589
  thu2 <- df_data %>% filter(dzien_tygodnia == "czwartek", usertype == "Customer" ) %>%nrow() # 47866 
  fri2 <- df_data %>% filter(dzien_tygodnia == "pi?tek", usertype == "Customer" ) %>% nrow() #55632
  sat2 <- df_data %>% filter(dzien_tygodnia == "sobota", usertype == "Customer" ) %>% nrow() #101069
  sun2 <- df_data %>% filter(dzien_tygodnia == "niedziela", usertype == "Customer" ) %>% nrow() #101412
  
  dzien <- c("Pon", "Pon", "Wt", "Wt", "Sr", "Sr", "Czw", "Czw", "Pt", "Pt", "So", "So", "Nd", "Nd")
  liczba_wypozyczen <- c(mon,mon2, tue,tue2, wed, wed2, thu, thu2, fri, fri2, sat, sat2, sun, sun2)
  Type <- c("sub", "cust", "sub", "cust", "sub", "cust", "sub", "cust", "sub", "cust", "sub", "cust", "sub", "cust")
  
  ramka <- data.frame(dzien, liczba_wypozyczen, Type)
  typeof(ramka)
  print(ramka)
  return(ramka)
}
data <- overall(df_data)
Dni_tygodnia <- factor(data$dzien, level = c('Pon', 'Wt', 'Sr', "Czw", "Pt", "So", "Nd"))
ggplot(data, aes(x= Dni_tygodnia, y=liczba_wypozyczen, fill= Type)) + geom_col()

# badanie popularnosci wypozyczen na poszczegolny dzien tygodnia dla mieszkancow w marcu (mieszkancow = subskrybenci) 
marzec_sub <- function(df_marzec18){
  df_marzec18$data_jazdy <- as.Date(df_marzec18$starttime)
  df_marzec18$dzien_tygodnia <- weekdays(df_marzec18$data_jazdy)
  df_marzec18$podzial <- as.numeric(strftime(df_marzec18$starttime, "%d"))
  
  df_marzec18 <- filter(df_marzec18, podzial <= 28)
  
  mon_mar <- df_marzec18 %>% filter(dzien_tygodnia == "poniedzia?ek", usertype == "Subscriber") %>% nrow()
  tue_mar <- df_marzec18 %>% filter(dzien_tygodnia == "wtorek", usertype == "Subscriber") %>% nrow()
  wed_mar <- df_marzec18 %>% filter(dzien_tygodnia == "?roda", usertype == "Subscriber") %>% nrow()
  thu_mar <- df_marzec18 %>% filter(dzien_tygodnia == "czwartek", usertype == "Subscriber") %>%nrow()
  fri_mar <- df_marzec18 %>% filter(dzien_tygodnia == "pi?tek", usertype == "Subscriber") %>% nrow()
  sat_mar <- df_marzec18 %>% filter(dzien_tygodnia == "sobota", usertype == "Subscriber") %>% nrow()
  sun_mar <- df_marzec18 %>% filter(dzien_tygodnia == "niedziela", usertype == "Subscriber") %>% nrow()
  
  return(c( mon_mar, tue_mar, wed_mar, thu_mar, fri_mar, sat_mar, sun_mar)) # wyniki to 146193 138173  86981 136620 111363 104584  93432
}
mar <- marzec_sub(df_marzec18)
barplot(names.arg = c("Pon", "Wt", "Sr", "Czw", "Pt", "So", "Nd" ), mar, col = rainbow(7), xlab = "dzie? tygodnia", ylab = "liczba wypo?ycze?"  )


# badanie popularnosci wypozyczen na poszczegolny dzien tygodnia dla mieszkancow w lipcu
lipiec_sub <- function(df_lipiec18){

  df_lipiec18$data_jazdy <- as.Date(df_lipiec18$starttime)
  df_lipiec18$dzien_tygodnia <- weekdays(df_lipiec18$data_jazdy )
  df_lipiec18$podzial <- as.numeric(strftime(df_lipiec18$starttime, "%d"))
  
  df_lipiec18 <- filter(df_lipiec18, podzial <= 28)

  mon_jul <- df_lipiec18 %>% filter(dzien_tygodnia == "poniedzia?ek", usertype == "Subscriber") %>% nrow()
  tue_jul <- df_lipiec18 %>% filter(dzien_tygodnia == "wtorek", usertype == "Subscriber") %>% nrow()
  wed_jul <- df_lipiec18 %>% filter(dzien_tygodnia == "?roda", usertype == "Subscriber") %>% nrow()
  thu_jul <- df_lipiec18 %>% filter(dzien_tygodnia == "czwartek", usertype == "Subscriber") %>% nrow()
  fri_jul <- df_lipiec18 %>% filter(dzien_tygodnia == "pi?tek", usertype == "Subscriber") %>% nrow()
  sat_jul <- df_lipiec18 %>% filter(dzien_tygodnia == "sobota", usertype == "Subscriber") %>% nrow()
  sun_jul <- df_lipiec18 %>% filter(dzien_tygodnia == "niedziela", usertype == "Subscriber") %>% nrow()
  
  
  return(c( mon_jul, tue_jul, wed_jul, thu_jul, fri_jul, sat_jul, sun_jul)) 
}
lip <- lipiec_sub(df_lipiec18) # wyniki: 222361 223616 209761 252364 215093 175359 153999
barplot(names.arg = c("Pon", "Wt", "Sr", "Czw", "Pt", "So", "Nd" ), lip, col = rainbow(7), xlab = "dzie? tygodnia", ylab = "liczba wypo?ycze?"  )

# badanie popularnosci wypozyczen na poszczegolny dzien tygodnia dla mieszkancow w pazdzierniku
pazdziernik_sub <- function(df_pazdziernik18){
  df_pazdziernik18$data_jazdy <- as.Date(df_pazdziernik18$starttime)
  df_pazdziernik18$dzien_tygodnia <- weekdays(df_pazdziernik18$data_jazdy)
  df_pazdziernik18$podzial <- as.numeric(strftime(df_pazdziernik18$starttime, "%d"))
  
  df_pazdziernik18 <- filter(df_pazdziernik18, podzial <= 28)
  
  mon_oct <- df_pazdziernik18 %>% filter(dzien_tygodnia == "poniedzia?ek", usertype == "Subscriber") %>% nrow()
  tue_oct <- df_pazdziernik18 %>% filter(dzien_tygodnia == "wtorek", usertype == "Subscriber") %>% nrow()
  wed_oct <- df_pazdziernik18 %>% filter(dzien_tygodnia == "?roda", usertype == "Subscriber") %>% nrow()
  thu_oct <- df_pazdziernik18 %>% filter(dzien_tygodnia == "czwartek", usertype == "Subscriber") %>%nrow()
  fri_oct <- df_pazdziernik18 %>% filter(dzien_tygodnia == "pi?tek", usertype == "Subscriber") %>% nrow()
  sat_oct <- df_pazdziernik18 %>% filter(dzien_tygodnia == "sobota", usertype == "Subscriber") %>% nrow()
  sun_oct <- df_pazdziernik18 %>% filter(dzien_tygodnia == "niedziela", usertype == "Subscriber") %>% nrow()
  
  return(c( mon_oct, tue_oct, wed_oct, thu_oct, fri_oct, sat_oct, sun_oct)) #223698 258622 262275 221919 239312 142659 158771
}
paz <- pazdziernik_sub(df_pazdziernik18)
barplot(names.arg = c("Pon", "Wt", "Sr", "Czw", "Pt", "So", "Nd" ), paz, col = rainbow(7), xlab = "dzie? tygodnia", ylab = "liczba wypo?ycze?"  )


# Badanie najpopularniejszych godzin wypozyczen (szukamy co powoduje ze w dni powszednie jest najwiecej wypozyczen)
# dla marca okazalo sie ze wystepuja pewne problemy i nie wykorzystywalem go dla tych funkcji (funkcja strftime nie chciala dla niego dzialac)
godzina_wypozyczen_sub <- function(df_lipiec18, df_pazdziernik18){
  df_lipiec18$rent_hour <- as.numeric(strftime(df_lipiec18$starttime, "%H"))
  df_pazdziernik18$rent_hour <- as.numeric(strftime(df_pazdziernik18$starttime, "%H"))
  tog <- rbind(df_lipiec18, df_pazdziernik18)
  tog <- tog %>% filter(usertype == "Subscriber")
  
  tmp1 <- tog %>% filter(rent_hour >= 0&rent_hour < 2) %>% nrow()
  tmp2 <- tog %>%  filter(rent_hour >= 2 & rent_hour < 4) %>% nrow()
  tmp3 <- tog %>%  filter(rent_hour >= 4 & rent_hour < 6)  %>% nrow()
  tmp4 <- tog %>%  filter(rent_hour >= 6 & rent_hour < 8) %>% nrow() 
  tmp5 <- tog %>%  filter(rent_hour >= 8 & rent_hour < 10) %>% nrow()
  tmp6 <- tog %>%  filter(rent_hour >= 10 & rent_hour < 12) %>% nrow()
  tmp7 <- tog %>%  filter(rent_hour >= 12 & rent_hour < 14) %>% nrow()
  tmp8 <- tog %>%  filter(rent_hour >= 14 & rent_hour < 16) %>% nrow()
  tmp9 <- tog %>%  filter(rent_hour >= 16 & rent_hour < 18) %>% nrow()
  tmp10 <- tog %>%  filter(rent_hour >= 18 & rent_hour < 20) %>% nrow()
  tmp11 <- tog %>%  filter(rent_hour >= 20 & rent_hour < 22) %>% nrow()
  tmp12 <- tog %>%  filter(rent_hour >= 22 ) %>% nrow()
  
  return(c(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12))
  #wyniki: 45098  14888  29784 247097 507923 280549 335883 360240 558027 536992 264811 133710
}
tmp_sub <- godzina_wypozyczen_sub(df_lipiec18, df_pazdziernik18)
barplot(names.arg = c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24" ), tmp_sub, col=rainbow(2), xlab = "dwugodzinne przedzialy", ylab = "ilosc wypozyczen")


godzina_wypozyczen_cust <- function(df_lipiec18, df_pazdziernik18){
  df_lipiec18$rent_hour <- as.numeric(strftime(df_lipiec18$starttime, "%H"))
  df_pazdziernik18$rent_hour <- as.numeric(strftime(df_pazdziernik18$starttime, "%H"))
  tog <- rbind(df_lipiec18, df_pazdziernik18)
  tog <- tog %>% filter(usertype == "Customer")
  
  tmp1 <- tog %>% filter(rent_hour >= 0&rent_hour < 2) %>% nrow()
  tmp2 <- tog %>%  filter(rent_hour >= 2 & rent_hour < 4) %>% nrow()
  tmp3 <- tog %>%  filter(rent_hour >= 4 & rent_hour < 6)  %>% nrow()
  tmp4 <- tog %>%  filter(rent_hour >= 6 & rent_hour < 8) %>% nrow() 
  tmp5 <- tog %>%  filter(rent_hour >= 8 & rent_hour < 10) %>% nrow()
  tmp6 <- tog %>%  filter(rent_hour >= 10 & rent_hour < 12) %>% nrow()
  tmp7 <- tog %>%  filter(rent_hour >= 12 & rent_hour < 14) %>% nrow()
  tmp8 <- tog %>%  filter(rent_hour >= 14 & rent_hour < 16) %>% nrow()
  tmp9 <- tog %>%  filter(rent_hour >= 16 & rent_hour < 18) %>% nrow()
  tmp10 <- tog %>%  filter(rent_hour >= 18 & rent_hour < 20) %>% nrow()
  tmp11 <- tog %>%  filter(rent_hour >= 20 & rent_hour <22) %>% nrow()
  tmp12 <- tog %>%  filter(rent_hour >= 22) %>% nrow()
  
  return(c(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12))
}

tmp_cust <- godzina_wypozyczen_cust(df_lipiec18, df_pazdziernik18)
barplot(names.arg = c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24" ), tmp_cust, col=rainbow(2), xlab = "dwugodzinne przedzialy", ylab = "ilosc wypozyczen")
