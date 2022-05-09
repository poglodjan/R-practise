test <- read.csv("blkprm.csv", TRUE, fileEncoding = "UTF-8")
kons <- read.csv("kons.csv", TRUE, fileEncoding = "UTF-8")

library("stringi")
library("stringr")
library("sqldf")
library("dplyr")

n <- nrow(test)
m <- ncol(test)
b <- ncol(kons)
colnames(test) <- c("Symbol JIRA", 'Nazwa JIRA', 'Status', 'Identyfikator kontraktowania', 'KonsultantBL',
                    'FirmaBL', 'Data logowania', 'Godziny', 'Komentarz', 'Rola', 'Nazwa produktu', 'Nazwa projektu')
osoba <- kons[3]
firma <- kons[7]
kons[1] <- osoba
kons[2] <- firma
kons[3:b] <- NULL
colnames(kons) <- c("KonsultantBL","firma")

Project <- test[1]
IssueKey <- test[2]
IssueSummary <- test[3]
IssueType <- test[4]
IssueKey_ParentIssue <- test[5]
IssueType_ParentIssue <- test[6]
Epic <- test[7]
Priority <- test[8]
Status <- test[9]
DueDate <- test[10]
Created <- test[11]
Updated <- test[12]
Resolution <- test[13]
Reporter <- test[14]
Assignee <- test[15]
Components <- test[16]
AffectsVersions <- test[17]
FixVersion <- test[18]
Labels <- test[19]
OriginalEstimate <- test[20]
OriginalEstimate <- test[21]
RemainingEstimate <- test[22]
RemainingEstimate <- test[23]
sumTimeSpent <- test[24]
sumTimeSpentH <- test[25]
Identyfikatorkontraktowania <- test[26]
ParenteHour <- test[27]
WorklogAuthor_Username <- test[28]
WorklogAuthor_FullName <- test[29]
Created <- test[30]
WorklogDate_WorklogAuthor <- test[31]
TZ_WorklogAuthor <- test[32]
WorklogDate_System <- test[33]
TimeLogged <- test[34]
TimeLoggedH <- test[35]
WorkDescription <- test[36]
Rola <- test[37]

ncol <- ncol(test) 
test[1] <- IssueKey
test[2] <- IssueSummary
test[3] <- Status
test[4] <- Identyfikatorkontraktowania  
test[5] <- WorklogAuthor_FullName
test[6] <- ""
test[7] <- WorklogDate_System
test[8] <- TimeLoggedH
test[9] <- WorkDescription
test[10] <- Rola
test[11] <- ""
test[12] <- ""
test[13:m] <- NULL

# nazwa projektu
for (row in 1:n){
  napis <- IssueSummary[row,1]
  fixed <- stri_list2matrix(stri_split_fixed(napis, c(']'), omit_empty=TRUE))
  fixed <- stri_list2matrix(stri_split_fixed(fixed, c('['), omit_empty=TRUE))
  test[row,12] <- fixed[1][1]
}

# nazwa produktu
for (row in 1:n){
  napis2 <- Identyfikatorkontraktowania[row,1]
  fixed2 <- stri_list2matrix(stri_split_fixed(napis2, c('|'), omit_empty=TRUE))
  n_fixed2 <- nrow(fixed2)
  key <- fixed2[n_fixed2-1]
  key <- stri_list2matrix(stri_split_fixed(key, c(':')))
  test[row,11] <- key[2][1]
}

#merge
merge <- test %>% left_join(kons, by="KonsultantBL")
merge$FirmaBL <- merge$firma
merge$firma <- NULL
test <- merge
produkty <- unique(test["Nazwa projektu"])
rownames(test) <- NULL

write.csv(test, file = sprintf("%s","Worklogall.csv"))

##################################################

# podział
colnames(test) <- c("SymbolJIRA", 'NazwaJIRA', 'Status', 'Identyfikatorkontraktowania', 'KonsultantBL',
                    'FirmaBL', 'Datalogowania', 'Godziny', 'Komentarz', 'Rola', 'Nazwaproduktu', 'Nazwaprojektu')


podzial <- unique(as.data.frame(tolower(test$Nazwaprojektu)))
to_lower <- as.data.frame(tolower(unique(test$Nazwaprojektu)))
podzial <- as.data.frame(podzial)
p <- nrow(podzial)

zachowaj_projekty <- test$Nazwaprojektu
test$Nazwaprojektu <- tolower(test$Nazwaprojektu)

#zapis podzialów
for (i in 1:p){
  key2 <- podzial[i,1]
  colnames(test) <- c("SymbolJIRA", 'NazwaJIRA', 'Status', 'Identyfikatorkontraktowania', 'KonsultantBL','FirmaBL', 'Datalogowania', 'Godziny', 'Komentarz', 'Rola', 'Nazwaproduktu', 'Nazwaprojektu')
  
  if(grepl(".", key2 ,fixed=T)==FALSE){
    new_file <- sqldf(sprintf("SELECT * FROM test WHERE Nazwaprojektu== '%s' ", key2))
  }else{
    next
  }
  
  ncolf <- ncol(new_file)
  nrowf <- nrow(new_file)
  colnames(new_file) <- c("Symbol JIRA", 'Nazwa JIRA', 'Status', 'Identyfikator kontraktowania', 'Konsultant BL','Firma BL', 'Data logowania', 'Godziny', 'Komentarz', 'Rola', 'Nazwa produktu', 'Nazwa projektu')
  
  file_produkty <- new_file[12]
  
  for(j in 1:nrowf) {
    napis <- new_file[j,2]
    fixed <- stri_list2matrix(stri_split_fixed(napis, c(']'), omit_empty=TRUE))
    fixed <- stri_list2matrix(stri_split_fixed(fixed, c('['), omit_empty=TRUE))
    file_produkty[j,1] <- fixed[1][1]
  }
  
  napis4 <- new_file[1,2]
  #sprawdzam czy projekty są nazwane
  if (grepl("[", napis4 ,fixed=T)==TRUE){
    check <- TRUE
  }else{
    check <- FALSE
  }
  
  new_file[12] <- file_produkty
  if(check==TRUE){
    write.csv(new_file, file = sprintf("%s%s", key2,".csv"))
  }
}

# reszta nienazwanych projektów
check <- IssueSummary
for( i in 1:n )
{
  napis3 <- IssueSummary[i,1]
  if(grepl("[", napis3 ,fixed=T)==T){
    check[i,1] <- TRUE
  }else{
    check[i,1] <- FALSE
  }
}

test[13] <- check
colnames(test) <- c("Symbol JIRA", 'Nazwa JIRA', 'Status', 'Identyfikator kontraktowania', 'Konsultant BL',
                    'Firma BL', 'Data logowania', 'Godziny', 'Komentarz', 'Rola', 'Nazwa produktu', 'Nazwa projektu', 'st')

if(nrow(table(check)) >1){
  rest <- sqldf("SELECT * FROM test WHERE st == 'FALSE' ")
}
rest[13]<- NULL
test[13]<- NULL

if (nrow(rest)>1){
  write.csv(rest, file = sprintf("%s","nienazwane projekty.csv"))
}

#powrót
test$Nazwaprojektu <- zachowaj_projekty



