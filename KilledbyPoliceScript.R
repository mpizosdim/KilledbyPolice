rm(list=ls())
library(stringr)

site2013 <- 'http://www.killedbypolice.net/kbp2013.html'
site2014 <- 'http://www.killedbypolice.net/kbp2014.html'
site2015 <- 'http://www.killedbypolice.net/'

##------------- 2013 ------------------
x <- readLines(site2013)
z<- gsub(pattern='(.*<TR><TD><center>)(.*)(<td><font size=2>.*)',replacement='\\2',x=x[31:807])
State <- regmatches(z , gregexpr("(?<=<td>)\\w+(?=<td>)" , z , perl = T) )
DatesT <- substring(z,5,regexpr('2013',z)+3)
Dates <- gsub(')','',DatesT)
SexT<- gsub(pattern='(.*<td>)(.*)(<td>.*)',replacement='\\2',x=z)
SexT <- substring(SexT,1,4)
Sex <- gsub(' ','',SexT)
AgeT<- gsub('\t','',z)
AgeT <- gsub(' ','',AgeT)
Age <- str_sub(AgeT,-2,-1)

##------------- 2014 ------------------

x <- readLines(site2014)
z<- gsub(pattern='(.*<TR><TD><center>)(.*)(<td><font size=2>.*)',replacement='\\2',x=x[31:1143])
State <- regmatches(z , gregexpr("(?<=<td>)\\w+(?=<td>)" , z , perl = T) )
DatesT <- substring(z,5,regexpr('2014',z)+3)
Dates <- gsub(')','',DatesT)
SexT<- gsub(pattern='(.*<td>)(.*)(<td>.*)',replacement='\\2',x=z)
SexT <- substring(SexT,1,4)
Sex <- gsub(' ','',SexT)
AgeT<- gsub('\t','',z)
AgeT <- gsub(' ','',AgeT)
Age <- str_sub(AgeT,-2,-1)

##------------- 2015 ------------------

x <- readLines(site2015)
z<- gsub(pattern='(.*<TR><TD><center>)(.*)(<TD><font size=2>.*)',replacement='\\2',x)
z<-z[31:which(z=="<script type=\"text/javascript\">")-2]
State <- regmatches(z , gregexpr("(?<=<td>)\\w+(?=<td>)" , z , perl = T) )
DatesT <- substring(z,5,regexpr('2015',z)+3)
Dates <- gsub(')','',DatesT)
SexT<- gsub(pattern='(.*<td>)(.*)(<td>.*)',replacement='\\2',x=z)
SexT <- substring(SexT,1,4)
Sex <- gsub(' ','',SexT)
AgeT<- gsub('\t','',z)
AgeT <- gsub(' ','',AgeT)
Age <- str_sub(AgeT,-2,-1)

