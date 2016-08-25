library(RCurl)
library(reshape2)
library(plyr)


#loading data

#loading data from github repository

url <- "https://raw.githubusercontent.com/pgaska88/project/project/tourismCopy.csv"
tourism1 <- getURL(url, ssl.verifypeer=0L, followlocation=1L)
tourism1 <- read.csv(text=tourism1,check.names = FALSE,stringsAsFactors = FALSE)
tourism1 <- tourism1[,c(-1,-2)]

url<-"https://raw.githubusercontent.com/pgaska88/project/project/gdpRegionsCopy.csv"
gdpRegions1 <- getURL(url, ssl.verifypeer=0L, followlocation=1L)
gdpRegions1 <- read.csv(text=gdpRegions1,check.names = FALSE,stringsAsFactors = FALSE)
gdpRegions1<-gdpRegions1[,c(-1,-2)]

url<-"https://raw.githubusercontent.com/pgaska88/project/project/nuts.csv"
nuts1<- getURL(url, ssl.verifypeer=0L, followlocation=1L)
nuts1<- read.csv(text=nuts1,stringsAsFactors = FALSE)
nuts1<-nuts1[,c(-1,-2)]

url<-"https://raw.githubusercontent.com/pgaska88/project/project/countries.csv"
countries<- getURL(url, ssl.verifypeer=0L, followlocation=1L)
countries<- read.csv(text=countries,stringsAsFactors = FALSE)
countries<-countries[,c(-1)]


#ANALYSIS

url <- "https://raw.githubusercontent.com/pgaska88/project/project/datAnalysis.csv"
dat2m <- getURL(url, ssl.verifypeer=0L, followlocation=1L)
dat2m <- read.csv(text=dat2m,check.names = FALSE)
dat2m<-dat2m[,-1]
dat2m$df<-as.factor(dat2m$df)

#MAP

#row-binding the dataframes
df2 <- rbind(tourism1[,c(1:8,9,10,11)], gdpRegions1[,c(1:8,9,10,11)])
#transforming the dataframe into 3 columns
df2 <- melt(df2, id.vars=c("region", "unit", "df"))
#merging with region labels
t<-merge(df2, nuts1[,5:7], by.x="region",by.y="NUTScode")
#getting country code from region code
t$country<- substr(t$region,1,2)