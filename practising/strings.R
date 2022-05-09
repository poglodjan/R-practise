library(stringr)
x <- c("why", "video", "cross", "extra", "deal", "authority")
str_length(x)
str_c(x, collapse=",")
str_sub(x,1,2)
str_subset(x, "[aeiou]")

prob1 <- stri_c("[eDOR] Raporty")

stri_length(nap)
prob2 <- stri_c("T:ROZWÓJ|S:eDoręczenia|U:3/DRUC/2019|Z:null|Z:new|Z:PK-1542|Z:Rozwój i modyfikacja dostarczonych modułów Systemu Komunikacyjnego eDoręczeń|P:PMEDOR-490 Aktualizacja dokumentu analizy biznesowej w zakresie modyfikowanych modułów Systemu Komunikacyjnego |M:503-054-2-100-0-2-017-1")

stri_split(prob1, "]")
?stri_split

if ((stri_detect_fixed(prob1, '[')) | (stri_detect_fixed(prob1, ']'))){
  stri_split(prob1)
}

stri_split_fixed('a_b_c__d', '_', omit_empty=TRUE)
stri_split_fixed('a_b_c__d', '_', n=2, tokens_only=TRUE)
stri_split_fixed('a_b_c__d', '_', n=4, omit_empty=TRUE, tokens_only=TRUE)

fixed <- stri_list2matrix(stri_split_fixed('[eDOR] Raporty', c(']'), omit_empty=TRUE))
fixed <- stri_list2matrix(stri_split_fixed(fixed, c('['), omit_empty=TRUE))
fixed[1][1]

fixed2 <- stri_list2matrix(stri_split_fixed('T:ROZWÓJ|S:eDoręczenia|U:3/DRUC/2019|Z:null|Z:new|Z:PK-1542|Z:Rozwój i modyfikacja dostarczonych modułów Systemu Komunikacyjnego eDoręczeń|P:PMEDOR-490 Aktualizacja dokumentu analizy biznesowej w zakresie modyfikowanych modułów Systemu Komunikacyjnego |M:503-054-2-100-0-2-017-1', c('|'), omit_empty=TRUE))
fixed2 <- stri_list2matrix(stri_split_fixed('T:ROZWÓJ|S:eDoręczenia|U:3/DRUC/2019|Z:null|Z:new|Z:PK-1542|Z:Rozwój i modyfikacja dostarczonych modułów Systemu Komunikacyjnego eDoręczeń|P:PMEDOR-490 Aktualizacja dokumentu analizy biznesowej w zakresie modyfikowanych modułów Systemu Komunikacyjnego |M:503-054-2-100-0-2-017-1', c(':'), omit_empty=TRUE))
n_fixed2 <- nrow(fixed2)
fixed2[n_fixed2-1]
