# Autor Jan Pogłód
# Przetwarzanie i analizowanie danych przy uzyciu różnych bibliotek wykorzystywanych w R
# Processing and analysis data frames using some R lbraries

options(stringsAsFactors=FALSE)
Tags <- read.csv("Tags.csv")
Badges <- read.csv("Badges.csv")
Comments <- read.csv("Comments.csv")
PostLinks <- read.csv("PostLinks.csv")
Posts <- read.csv("Posts.csv")
Users <- read.csv("Users.csv")
Votes <- read.csv("Votes.csv")

library("sqldf")
library("dplyr")
library("data.table")
library("microbenchmark")

#####################################################
# --- 1)

df_sql_1 <- function(Tags){
  df1 <- sqldf("SELECT Count, TagName FROM Tags WHERE Count > 1000 ORDER BY Count DESC")
  return(df1)
}

df_base_1 <- function(Tags){
  x <- Tags[c("TagName", "Count")]
  x <- as.data.frame(na.omit(Tags[ x$Count > 1000, c("Count", "TagName") ], drop=FALSE))
  x <- x[order( x$Count, decreasing = TRUE ), ]
  rownames(x) <- NULL
  return(x)
}

df_table_1 <- function(Tags){
  t <- as.data.table(Tags)
  t <- t[ t$Count>1000, ]
  t[ ,c("ExcerptPostId","WikiPostId","Id" )] <- NULL
  t <- t[order(t$Count, decreasing = TRUE), ]
  t <- t[ ,c("Count", "TagName")]
  rownames(t) <- NULL
  t$Count <- as.integer(t$Count)
  return(t)
}

df_dplyr_1 <- function(Tags){
  d <- Tags %>% filter(Count > 1000)
  d <- d %>% select(c("TagName","Count")) 
  d <- d %>% arrange(desc(Count))
  return(d)
}

sqldf1 <- df_sql_1(Tags)
base1 <- df_base_1(Tags)
table1 <- df_table_1(Tags)
dplyr1 <- df_dplyr_1(Tags)

dplyr::all_equal(sqldf1,base1,table1) # Otrzymujemy TRUE
dplyr::all_equal(sqldf1,base1,dplyr1) # Otrzymujemy TRUE

#sprawdzenie wydajności
microbenchmark::microbenchmark(sqldf1,base1,table1,dplyr1)

##########################################################
# --- 2)

df_sql_2 <- function(Users,Posts){
  df1 <- sqldf("SELECT Location, COUNT(*) AS Count
  FROM (
  SELECT Posts.OwnerUserId, Users.Id, Users.Location
  FROM Users
  JOIN Posts ON Users.Id = Posts.OwnerUserId
  )
  WHERE Location NOT IN ('')
  GROUP BY Location
  ORDER BY Count DESC
  LIMIT 10")
  return(df1)
}

df_base_2 <- function(Users,Posts){
  u <- Users[c("Id", "Location")]
  u <- u[u$Location != "", ]
  p <- Posts["OwnerUserId"]
  colnames(p) <- "Id"
  merge <- merge(u,p,"Id")
  agg <- aggregate(merge[1], merge["Location"], length)
  x <- agg[order( agg$Id, decreasing = TRUE ), ]
  x <- head(x,10)
  rownames(x) <- NULL
  colnames(x) <- c("Location", "Count")
  return(x)
}

df_table_2 <- function(Users, Posts){
  u <- as.data.table(Users[c("Id","Location")])
  u <- u[ u$Location!="", ]
  p <- as.data.table(Posts["OwnerUserId"])
  setnames(p, 'Id')
  merge <- merge(u,p, by="Id") 
  table <- merge[, .(Count = .N), by = Location]
  table <- head(table[order(table$Count, decreasing = TRUE), ],10)
  return(table)
}

df_dplyr_2 <- function(Users, Posts){
  users <- Users %>% select(c("Id","Location"))
  posts <- Posts %>% select("OwnerUserId")
  users <- users %>% filter(Location != "") 
  colnames(posts) <- "Id"
  merge <- users %>% inner_join(posts, by="Id")
  df <- merge %>% group_by(Location) %>% summarise(Count=n()) 
  df <- df %>% arrange(desc(Count)) 
  df <- head(df,10)
  return(df)
}

sqldf2 <- df_sql_2(Users,Posts)
base2 <- df_base_2(Users, Posts)
table2 <- df_table_2(Users, Posts)
dplyr2 <- df_dplyr_2(Users, Posts)

# sprawdzenie poprawności 
dplyr::all_equal(sqldf2,table2,dplyr2) # Otrzymujemy TRUE
dplyr::all_equal(sqldf2,table2,base2) # Otrzymujemy TRUE

#sprawdzenie wydajności
microbenchmark::microbenchmark(sqldf2,base2,table2,dplyr2)

##########################################################
# --- 3)

df_sql_3 <- function(Badges){
  df1 <- sqldf("SELECT Year, SUM(Number) AS TotalNumber
  FROM (
  SELECT
  Name,
  COUNT(*) AS Number,
  STRFTIME('%Y', Badges.Date) AS Year
  FROM Badges
  WHERE Class = 1
  GROUP BY Name, Year
  )
  GROUP BY Year
  ORDER BY TotalNumber")
  return(df1)
}

df_base_3 <- function(Badges){
  badges <- as.data.frame( na.omit( Badges[ Badges$Class == "1", c("Date", "Name") ], drop=FALSE) )
  date <- format( as.Date( badges[,1] ),"%Y" )
  badges$Date <- date
  colnames(badges) <- c("Year","Name")
  agg <- aggregate(badges, badges[c("Year")], length) 
  agg$Name <- NULL
  colnames(agg) <- c("Year","TotalNumber") 
  agg <- agg[order( agg$TotalNumber, decreasing = FALSE ), ]
  return(agg)
}

df_table_3 <- function(Badges){
  table <- as.data.table(Badges[c("Date","Name","Class")])
  table <- table[table$Class=="1"]
  table[, Class:=NULL]
  date <- table$Date
  year <- as.data.table( year( date ) )
  setnames( year, "Year" )
  setnames(table, c( "Year","Name" ) )
  table[ ,1] <- lapply(year, as.character, stringsAsFactors=FALSE)
  df <- table[, .(Count = .N), by = Year]
  df <- (df[order(df$Count, decreasing = FALSE), ])
  setnames( df, c("Year","TotalNumber") )
  return(df)
}

df_dplyr_3 <- function(Badges){
  badges <- Badges %>% filter(Class == "1")
  badges <- badges %>% select(Name,Date)
  date <- badges %>% select(Date)
  year <- format(as.Date(date[ ,1]), "%Y")
  matrix_year <- year %>% as_tibble()
  badges$Date <- matrix_year
  df <- badges %>% group_by(Date) %>% summarise(TotalNumber=n())
  colnames(df) <- c("Year","TotalNumber")
  df <- df %>% arrange(TotalNumber) 
  return(df)
}

sqldf3 <- df_sql_3(Badges)
base3 <- df_base_3(Badges)
table3 <- df_table_3(Badges)
dplyr3 <- df_dplyr_3(Badges)

# sprawdzenie poprawności
dplyr::all_equal(sqldf3,base3,dplyr3) # Otrzymujemy TRUE
dplyr::all_equal(sqldf3,table3,base3) # Otrzymujemy TRUE

#sprawdzenie wydajności
microbenchmark::microbenchmark(sqldf3,base3,table3,dplyr3)

##########################################################
# --- 4)

df_sql_4 <- function(Users,Posts){
  df <- sqldf("SELECT
  Users.AccountId,
  Users.DisplayName,
  Users.Location,
  AVG(PostAuth.AnswersCount) as AverageAnswersCount
  FROM
  (
  SELECT
  AnsCount.AnswersCount,
  Posts.Id,
  Posts.OwnerUserId
  FROM (
  SELECT Posts.ParentId, COUNT(*) AS AnswersCount
  FROM Posts
  WHERE Posts.PostTypeId = 2
  GROUP BY Posts.ParentId
  ) AS AnsCount
  JOIN Posts ON Posts.Id = AnsCount.ParentId
  ) AS PostAuth
  JOIN Users ON Users.AccountId=PostAuth.OwnerUserId
  GROUP BY OwnerUserId
  ORDER BY AverageAnswersCount DESC, AccountId ASC
  LIMIT 10")
  return(df)
}

df_base_4 <- function(Users,Posts){
  p <- Posts[ ( is.na(Posts$PostTypeId)==FALSE & Posts$PostTypeId==2 ), ]
  posts <- p["ParentId"]
  Id <- Posts[c("Id","OwnerUserId")]
  user <- Users[c("AccountId","Location","DisplayName")]
  colnames(user) <- c("OwnerUserId","Location","DisplayName")
  AnsCount <- aggregate(posts, posts["ParentId"], length)
  colnames(AnsCount) <- c("Id","TotalNumber")
  PostAuth <- merge(Id,AnsCount,by="Id")
  merge <- merge(user,PostAuth,by="OwnerUserId")
  df <- merge[is.na(merge$OwnerUserId) == FALSE,]
  df["Id"] <- NULL
  average <- df[c("OwnerUserId","TotalNumber")]
  average <- average[order( average$OwnerUserId ), ]
  mean <- as.data.frame(aggregate(average["TotalNumber"], average["OwnerUserId"], mean))
  df <- merge(df, mean, by="OwnerUserId")
  df <- df[ c( "OwnerUserId","DisplayName","Location","TotalNumber.y" ) ]
  colnames(df) <- c("AccountId","DisplayName","Location","AverageAnswersCount")
  df <- head( df[ order( df$AverageAnswersCount,decreasing = TRUE ), ], 10 )
  rownames(df) <- NULL
  return(df)
}

df_table_4 <- function(Users, Posts){
  pt <- as.data.table(Posts)
  Id <- as.data.table(Posts[c("Id","OwnerUserId")])
  user <- as.data.table(Users[c("AccountId","Location","DisplayName")])
  posts <- pt[ pt$PostTypeId==2, ]
  setnames(user, c("OwnerUserId","Location","DisplayName") )
  AnsCount <- posts[, .(Count = .N), by = ParentId]
  setnames(AnsCount, c("Id","TotalNumber"))
  PostAuth <- merge( Id, AnsCount, by="Id" )
  merge <- merge( user, PostAuth, by="OwnerUserId" )
  df <- merge[ is.na( merge$OwnerUserId ) == FALSE, ]
  df[ , Id:=NULL ]
  df[ ,1] <- lapply( df[ ,1], as.character, stringsAsFactors=FALSE )
  average <- df
  average <- average[, Location:=NULL]
  average <- average[, DisplayName:=NULL]
  average <- average[order( average$OwnerUserId ), ]
  mean <- average[ , (mean=mean(TotalNumber) ), by = OwnerUserId]
  table <- merge[is.na(merge$OwnerUserId) == FALSE, ]
  table[ ,1] <- lapply(table[ ,1], as.character, stringsAsFactors=FALSE)
  res <- merge(table, mean, by="OwnerUserId")
  res <- res[order(-res$V1),]
  res[, Id:=NULL]
  res[, TotalNumber:=NULL]
  setnames(res, c("AccountId","DisplayName","Location","AverageAnswersCount"))
  res <- head( res,10 )
  res[ ,c("Location", "DisplayName") := .(DisplayName, Location)]
  res$AccountId <- as.integer(res$AccountId)
  rownames(res) <- NULL
  return(res)
}

df_dplyr_4 <- function(Users, Posts){
  p <- Posts %>% filter(PostTypeId==2)
  posts <- p %>% select(ParentId)
  Id <- Posts %>% select(Id,OwnerUserId)
  user <- Users %>% select(AccountId,Location,DisplayName)
  colnames(user) <- c("OwnerUserId","Location","DisplayName")
  AnsCount <- posts %>% group_by(ParentId) %>% summarise(TotalNumber=n())
  colnames(AnsCount) <- c("Id","TotalNumber")
  PostAuth <- Id %>% inner_join(AnsCount, by="Id")
  merge <- user %>% inner_join(PostAuth, by="OwnerUserId")
  merge <- merge[is.na(merge$OwnerUserId) == FALSE,]
  average <- merge %>% select(OwnerUserId,TotalNumber) %>% as_tibble()
  average <- average %>% arrange(OwnerUserId)
  mean <- average %>% group_by(OwnerUserId) %>% summarise(n = n(),TotalNumber = mean(TotalNumber, na.rm = TRUE))
  df <- merge %>% inner_join(mean, by="OwnerUserId")
  df <- df %>% select(OwnerUserId,DisplayName,Location,TotalNumber.y)
  colnames(df) <- c("AccountId","DisplayName","Location","AverageAnswersCount")
  df <- df %>% arrange(desc(AverageAnswersCount),AccountId)
  df <- head( df, 10 )
  rownames(df) <- NULL
  return(df)
}

sqldf4 <- df_sql_4(Users,Posts)
base4 <- df_base_4(Users, Posts)
table4 <- df_table_4(Users, Posts)
dplyr4 <- df_dplyr_4(Users, Posts)

#sprawdzenie poprawności
dplyr::all_equal(sqldf4,base4,dplyr4) # Otrzymujemy TRUE
dplyr::all_equal(sqldf4,base4,table4) # Otrzymujemy TRUE

#sprawdzenie wydajności
microbenchmark::microbenchmark(sqldf4,base4,table4,dplyr4)

##########################################################
# --- 4)

df_sql_5 <- function(Votes,Posts){
  df <- sqldf("SELECT Posts.Title, Posts.Id,
  STRFTIME('%Y-%m-%d', Posts.CreationDate) AS Date,
  VotesByAge.Votes
  FROM Posts
  JOIN (
  SELECT
  PostId,
  MAX(CASE WHEN VoteDate = 'new' THEN Total ELSE 0 END) NewVotes,
  MAX(CASE WHEN VoteDate = 'old' THEN Total ELSE 0 END) OldVotes,
  SUM(Total) AS Votes
  FROM (
  SELECT
  PostId,
  CASE STRFTIME('%Y', CreationDate)
  WHEN '2021' THEN 'new'
  WHEN '2020' THEN 'new'
  ELSE 'old'
  END VoteDate,
  COUNT(*) AS Total
  FROM Votes
  WHERE VoteTypeId IN (1, 2, 5)
  GROUP BY PostId, VoteDate
  ) AS VotesDates
  GROUP BY VotesDates.PostId
  HAVING NewVotes > OldVotes
  ) AS VotesByAge ON Posts.Id = VotesByAge.PostId
  WHERE Title NOT IN ('')
  ORDER BY Votes DESC
  LIMIT 10")
  return(df)
}

sqldf5 <- df_sql_5(Votes,Posts)
microbenchmark::microbenchmark(sqldf5)

