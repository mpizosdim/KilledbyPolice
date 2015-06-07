rm(list=ls())
setwd('C:/Users/Dimitrios/Documents/Dimitris_general/R programming my projects/KilledbyPolice')
library(stringr)
require(lubridate)
library(doBy)
library(plyr)
library(grid)
library(ggplot2)
source('multiplot.R')
site2013 <- 'http://www.killedbypolice.net/kbp2013.html'
site2014 <- 'http://www.killedbypolice.net/kbp2014.html'
site2015 <- 'http://www.killedbypolice.net/'

##------------- 2013 ------------------
x1 <- readLines(site2013)
z1<- gsub(pattern='(.*<TR><TD><center>)(.*)(<td><font size=2>.*)',replacement='\\2',x=x1[31:807])
State2013 <- regmatches(z1 , gregexpr("(?<=<td>)\\w+(?=<td>)" , z1 , perl = T) )
DatesT1 <- substring(z1,5,regexpr('2013',z1)+3)
Dates2013 <- gsub(')','',DatesT1)
SexT1<- gsub(pattern='(.*<td>)(.*)(<td>.*)',replacement='\\2',x=z1)
SexT1 <- substring(SexT1,1,4)
Sex2013 <- gsub(' ','',SexT1)
AgeT1<- gsub('\t','',z1)
AgeT1 <- gsub(' ','',AgeT1)
Age2013 <- str_sub(AgeT1,-2,-1)

##------------- 2014 ------------------

x2 <- readLines(site2014)
z2<- gsub(pattern='(.*<TR><TD><center>)(.*)(<td><font size=2>.*)',replacement='\\2',x=x2[31:1143])
State2014 <- regmatches(z2 , gregexpr("(?<=<td>)\\w+(?=<td>)" , z2 , perl = T) )
DatesT2 <- substring(z2,5,regexpr('2014',z2)+3)
Dates2014 <- gsub(')','',DatesT2)
SexT2_1<- gsub(pattern='(.*<td>)(.*)(<TD>.*)',replacement='\\2',x=z2[1:400])
SexT2_1 <- substring(SexT2_1,1,4)
Sex2014_1 <- gsub(' ','',SexT2_1)

SexT2_2<- gsub(pattern='(.*<td>)(.*)(<td>.*)',replacement='\\2',x=z2[401:1113])
SexT2_2 <- substring(SexT2_2,1,4)
Sex2014_2 <- gsub(' ','',SexT2_2)
Sex2014<- c(Sex2014_1,Sex2014_2)
AgeT2<- gsub('\t','',z2)
AgeT2 <- gsub(' ','',AgeT2)
Age2014 <- str_sub(AgeT2,-2,-1)

##------------- 2015 ------------------

x3 <- readLines(site2015)
z3<- gsub(pattern='(.*<TR><TD><center>)(.*)(<TD><font size=2>.*)',replacement='\\2',x3)
z3<-z3[31:which(z3=="<script type=\"text/javascript\">")-2]
State2015 <- regmatches(z3 , gregexpr("(?<=<td>)\\w+(?=<td>)" , z3 , perl = T) )
DatesT3 <- substring(z3,5,regexpr('2015',z3)+3)
Dates2015 <- gsub(')','',DatesT3)
SexT3<- gsub(pattern='(.*<td>)(.*)(<TD>.*)',replacement='\\2',x=z3)
SexT3 <- substring(SexT3,1,4)
Sex2015 <- gsub(' ','',SexT3)
AgeT3<- gsub('\t','',z3)
AgeT3 <- gsub(' ','',AgeT3)
Age2015 <- str_sub(AgeT3,-2,-1)

Date <- c(Dates2013,Dates2014,Dates2015)
State <- c(State2013,State2014,State2015)
Sex_Race <- c(Sex2013,Sex2014,Sex2015)
Age <- c(Age2013,Age2014,Age2015)

## Cleaning and ordering Data
Date <- as.Date(Date,' %b %d, %Y')
Age <- as.numeric(Age)
SexT <- substring(Sex_Race,1,1)
Sex <- factor(SexT,levels=c('M','F'))
RaceT <- substring(Sex_Race,3,3)
Race <- factor(RaceT,levels=c('W','L','B','P','O','I','A'))
State <- as.character(State)    
State <- factor(State,levels=state.abb)
State2 <- state.name[match(State,state.abb)]
State2 <- tolower(State2)
Data <- data.frame(Date=Date, Age=Age, Race=Race,Sex=Sex, region=State2)


DeathsPerState <- summaryBy(Age~region,data=Data,FUN=c(length))
MeanAgePerState <- summaryBy(Age~region,data=Data,FUN=c(mean),na.rm=TRUE)
MeanAgePerState <- summaryBy(Age~region,data=Data,FUN=c(mean),na.rm=TRUE)

MeanAgePerState$region <- as.character(MeanAgePerState$region)
all_states <- map_data('state')
all_states <- all_states[all_states$region!="district of columbia",]
Total <- merge(all_states,DeathsPerState,by='region')

## plot US
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$Age.length),colour="white"
) + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Black to White Incarceration Rates \n Weighted by Relative Population" 
                             ,title = "State Incarceration Rates by Race, 2010", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())

## Histogram Age

ggplot(Data,aes(x=Age))+
    geom_histogram(aes(binwidth=.5),
                   colour='black',
                   fill='white')+
    geom_vline(aes(xintercept=mean(Age, na.rm=TRUE)),
               color='red',linetype='dashed',size=1)

## Histogram Age Sex
cdat <- summaryBy(Age~Sex,data=Data,FUN=c(mean),na.rm=TRUE)
cdat <- cdat[-3,]
ggplot(Data,aes(x=Age,fill=Sex))+
    geom_histogram(binwidth=.7, alpha=.8, position="identity")+
    geom_vline(data=cdat, aes(xintercept=Age.mean,  colour=Sex),
               linetype="dashed", size=1)


## pipe male~female

Male_FemaleN <- summaryBy(Age~Sex,data=Data,FUN=c(length))

pie <- ggplot(Male_FemaleN, aes(x = Age.length, fill = factor(Sex))) +
    geom_bar(width = 1)
pie + coord_polar()


bp<- ggplot(Male_FemaleN, aes(x="", y=Age.length, fill=Sex))+
    geom_bar(width = 1, stat = "identity")
pie1 <- bp + coord_polar("y", start=0)

## Race

RaceN <- summaryBy(Age~Race,data=Data,FUN=c(length))

bp<- ggplot(RaceN, aes(x="", y=Age.length, fill=Race))+
    geom_bar(width = 1, stat = "identity")
pie2 <- bp + coord_polar("y", start=0)

multiplot(pie1,pie2,cols=2)


## Time serie per year

Date <- summaryBy(Age~Date,data=Data,FUN=c(length))
Date$Date<-as.Date(Date$Date,'%Y-%m-%d')
Year2015N <- Date[year(Date$Date)==2015,]
Year2015N$Week <- week(Year2015N$Date)
Year2014N <- Date[year(Date$Date)==2014,]
Year2014N$Week <- week(Year2014N$Date)
Year2013N <- Date[year(Date$Date)==2013,]
Year2013N$Week <- week(Year2013N$Date)

Weekly2014 <- summaryBy(Age.length~Week,data=Year2014N,FUN=sum,na.rm=TRUE)
Weekly2014 <- Weekly2014[-c(49),]


Weekly2015 <- summaryBy(Age.length~Week,data=Year2015N,FUN=sum,na.rm=TRUE)
Weekly2015 <- Weekly2015[-c(24),]
Weekly2013 <- summaryBy(Age.length~Week,data=Year2013N,FUN=sum,na.rm=TRUE)
Weekly2013 <- Weekly2013[-c(37),]

WeekTemp1 <- merge(Weekly2013,Weekly2014,by='Week',all=TRUE)
WeekTemp1 <- WeekTemp1[-c(54),]
WeekTemp2 <- merge(WeekTemp1,Weekly2015,by='Week',all=TRUE)
WeekData <- WeekTemp2[-c(54),]
colnames(WeekData) <- c("Week", "2013",'2014','2015')



ggplot(data=WeekData,aes(x=Week,y=Kills,color=.))+
    geom_line(aes(x=Week,y = WeekData$'2013', col = "2013"),size=1)+
    geom_line(aes(x=Week,y = WeekData$'2014', col = "2014"),size=1)+
    geom_line(aes(x=Week,y = WeekData$'2015', col = "2015"),size=1)


#time series for colored ppl

Bppl <- summaryBy(Age~Date+Race,data=Data,FUN=c(length))
Bppl <- Bppl[!is.na(Bppl$Race),]
Bppl <- Bppl[!is.na(Bppl$Date),]
Bppl <- Bppl[Bppl$Race=='B',]


Year2015NB <- Bppl[year(Bppl$Date)==2015,]
Year2015NB$Week <- week(Year2015NB$Date)
Year2014NB <- Bppl[year(Bppl$Date)==2014,]
Year2014NB$Week <- week(Year2014NB$Date)
Year2013NB <- Bppl[year(Bppl$Date)==2013,]
Year2013NB$Week <- week(Year2013NB$Date)



Weekly2014 <- summaryBy(Age.length~Week,data=Year2014NB,FUN=sum,na.rm=TRUE)
Weekly2015 <- summaryBy(Age.length~Week,data=Year2015NB,FUN=sum,na.rm=TRUE)
Weekly2015 <- Weekly2015[-c(24),]
Weekly2013 <- summaryBy(Age.length~Week,data=Year2013NB,FUN=sum,na.rm=TRUE)

WeekTemp1 <- merge(Weekly2013,Weekly2014,by='Week',all=TRUE)
WeekTemp2 <- merge(WeekTemp1,Weekly2015,by='Week',all=TRUE)
colnames(WeekData) <- c("Week", "2013",'2014','2015')

ggplot(data=WeekData,aes(x=Week,y=Kills,color=.))+
    geom_line(aes(x=Week,y = WeekData$'2013', col = "2013"),size=1)+
    geom_line(aes(x=Week,y = WeekData$'2014', col = "2014"),size=1)+
    geom_line(aes(x=Week,y = WeekData$'2015', col = "2015"),size=1)


