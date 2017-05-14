library("gridExtra")
library("RGA")
library("ggplot2")

source("/Users/kimberlyhale/Documents/Side Projects/TSB/Analysis/Functions/AuthorizeGA.R")

#Pull data and order
ga_source <- get_ga(profileId = all.id, start.date = "2017-05-01",
                   end.date = "2017-05-11", metrics = c("ga:uniquePageviews"), 
                   dimensions = c("ga:pagePath","ga:dayOfWeek","ga:date","ga:year", 
                                  "ga:month", "ga:day", "ga:source"))
ga_source <- ga_source[order(-ga_source$uniquePageviews),]
ga_source$month <- as.numeric(ga_source$month)
ga_source$day <- as.numeric(ga_source$day)
ga_source$year <- as.numeric(ga_source$year)
ga_source$dayOfWeek <- as.numeric(ga_source$dayOfWeek)

ga_source$date_c <- paste("the-bridge/",ga_source$year,"/",ga_source$month,"/",ga_source$day, sep="")
ga_source.df$posted <- mapply(grepl, pattern=ga_source.df$date_c, x=ga_source.df$pagePath)


#Combine similar data sources
combine1 <- which(ga_source$source %in% agrep("facebook", ga_source$source, max=0.3, value= TRUE))
combine2 <- c(which(ga_source$source == "t.co"), which(ga_source$source %in% agrep("twitter", ga_source$source, max=0.1, value = TRUE)))
combine3 <- which(ga_source$source %in% agrep("google", ga_source$source, max=0.3, value=TRUE))
combine4 <- c(which(ga_source$source == "lnkd.in"), which(ga_source$source %in% agrep("linked", ga_source$source, max=0.1, value=TRUE)))

ga_source$source[combine1]<-"facebook.com"
ga_source$source[combine2]<-"twitter.com"
ga_source$source[combine3]<-"google.com"
ga_source$source[combine4]<-"linkedin.com"

ga_source.df <- aggregate(ga_source$uniquePageviews ~ ga_source$pagePath + ga_source$dayOfWeek 
                          + ga_source$date + ga_source$date_c + ga_source$source, data=ga_source, sum)
colnames(ga_source.df) <- c("pagePath","dayOfWeek", "date", "date_c", "source","uniquePageviews")
ga_source.df <- ga_source.df[order(-ga_source.df$uniquePageviews),]

ga_source.df$posted <- mapply(grepl, pattern=ga_source.df$date_c, x=ga_source.df$pagePath)
ga_source.df$day2 <- ga_source.df$date + 60*60*24
ga_source.df$pagePath2 <- ga_source.df$pagePath
#match pagePath to pagePath and date to date2
match_index <- which( outer(ga_source.df$pagePath, ga_source.df$pagePath2, "==") & 
         outer(ga_source.df$date, ga_source.df$day2, "=="), 
       arr.ind=TRUE)

ga_day1 <- ga_source.df[which(ga_source.df$posted==TRUE),]


sourcelist <- c("facebook.com", "twitter.com", "(direct)", "The Bridge")
ga_day1$source[which(!(ga_day1$source %in% sourcelist))]<-"other"

ga_day1.df <- aggregate(ga_day1$uniquePageviews ~ ga_day1$pagePath + 
                               ga_day1$dayOfWeek +  ga_day1$date + 
                               ga_day1$date_c + ga_day1$source , data=ga_day1, sum)
colnames(ga_day1.df) <- c("pagePath","dayOfWeek", "date", "date_c", "source","uniquePageviews")
ga_day1.df <- ga_day1.df[order(ga_day1.df$date, ga_day1.df$pagePath, ga_day1.df$uniquePageviews),]

ggplot(ga_day1.df, aes(x=date, y=uniquePageviews, colour=source)) + geom_line() 

library(data.table)
test <- as.data.table(ga_day1.df)

dat <- test[,.SD[which.max(uniquePageviews)], by=pagePath]


ggplot(ga_day1.df, aes(x=pagePath, y=uniquePageviews, colour=source)) + geom_col(position = "stack") +
       facet_grid(source~., scales="fixed") + theme(legend.position="bottom")
ggsave("Article Traffic on 1st Day by Source - 201601-201705.png")

ggplot(dat, aes(x=pagePath, y=uniquePageviews, colour=source)) + geom_col(position = "stack") +
  facet_grid(source~., scales="fixed") + theme(legend.position="bottom")
ggsave("Article Traffic on 1st Day by Max Source - 2.png")


ga_monday.df <- aggregate(ga_source$uniquePageviews ~ ga_source$pagePath + ga_source$dayOfWeek 
                          + ga_source$dateHour + ga_source$source, data=ga_source, sum)
colnames(ga_monday.df) <- c("pagePath","dayOfWeek", "dateHour", "source","uniquePageviews")
ga_monday.df <- ga_monday.df[order(-ga_monday.df$uniquePageviews),]

pagenames <- c("/the-bridge/2017/5/8/reviewing-the-myth-gap-what-happens-when-evidence-and-arguments-arent-enough",
               "/the-bridge/2017/5/8/reviewing-and-interviewing-war-stories",
               "/the-bridge/2017/5/8/an-extended-discussion-on-an-important-question-what-is-information-operations")
pagenames <- c("/the-bridge/2017/5/1/marching-together-but-not-in-time-reviewing-in-cadence",
               "/the-bridge/2017/5/1/reviewing-a-tale-of-two-navies")
pagenames <- c("/the-bridge/2017/4/24/african-solutions-to-african-problems-reviewing-composite-warfarethe-conduct-of-successful-ground-force-operations-in-africa"
               , "/the-bridge/2017/4/24/reviewing-a-history-of-warfare")
combine5 <- which(!(ga_monday.df$pagePath %in% pagenames))
ga_monday.df$pagePath[combine5]<-"other"
ga_mon_only.df <- aggregate(ga_monday.df$uniquePageviews ~ ga_monday.df$pagePath + ga_monday.df$dayOfWeek 
                          + ga_monday.df$dateHour + ga_monday.df$source, data=ga_monday.df, sum)
colnames(ga_mon_only.df) <- c("pagePath","dayOfWeek", "dateHour", "source","uniquePageviews")
ga_mon_only.df <- ga_mon_only.df[order(ga_mon_only.df$dateHour),]

ga_mon_only2.df <- ga_mon_only.df[which((ga_mon_only.df$pagePath %in% pagenames)),]
colnames(ga_mon_only2.df) <- c("pagePath","dayOfWeek", "dateHour", "source","uniquePageviews")
ga_mon_only2.df <- ga_mon_only2.df[order(ga_mon_only2.df$dateHour),]

sourcelist <- c("facebook.com", "twitter.com", "google.com", "(direct)")
others <- which(!(ga_mon_only2.df$source %in% sourcelist))

ga_mon_only2.df$source[others]<-"other"

ga_mon_only2.df <- aggregate(ga_mon_only2.df$uniquePageviews ~ ga_mon_only2.df$source + 
                               ga_mon_only2.df$dayOfWeek +  ga_mon_only2.df$dateHour + 
                               ga_mon_only2.df$pagePath, data=ga_mon_only2.df, sum)


colnames(ga_mon_only2.df) <- c("Traffic Source","dayOfWeek", "Time of Day (EST)", "pagePath", "Unique Pageviews")

sub1 <-which(ga_mon_only2.df$pagePath == "/the-bridge/2017/5/8/reviewing-the-myth-gap-what-happens-when-evidence-and-arguments-arent-enough") 
ga_mon_only2.df$pagePath[sub1] <- "Reviewing the Myth Gap"
sub2 <-which(ga_mon_only2.df$pagePath == "/the-bridge/2017/5/8/reviewing-and-interviewing-war-stories") 
ga_mon_only2.df$pagePath[sub2] <- "Reviewing & Interviewing War Stories"
sub3 <-which(ga_mon_only2.df$pagePath == "/the-bridge/2017/5/8/an-extended-discussion-on-an-important-question-what-is-information-operations") 
ga_mon_only2.df$pagePath[sub3] <- "Discussion on Information Ops"


#Plot
ggplot(ga_mon_only2.df, aes(`Time of Day (EST)`, y=`Unique Pageviews`, colour=`Traffic Source`)) + geom_line() + 
  facet_grid(pagePath ~., scales = "fixed") + theme(legend.position="bottom")
ggsave("Traffic by Source-4-24-2017.png")

#Look at other monday articles
May1 <- ga_monday[which(ga_monday$month == 5 & ga_monday$day == 1),]
May1_posted <- May1[grep("2017/5/1/", May1$pagePath),]

ga_monday$posted <- NA
ga_monday$posted[grep("2017/4/24", ga_monday$pagePath)] <- "2017/4/24"

dates <- c("2017/4/17", "2017/4/10", "2017/4/3", "2017/3/27", "2017/3/20", "2017/3/13", 
           "2017/3/6", "2017/2/27", "2017/2/20", "2017/2/13", "2017/2/6", "2017/1/30")
  for(date_i in dates){
  ga_monday$posted[grep(date_i, ga_monday$pagePath)] <- date_i
  }



ggplot(May1_posted, aes(x="", y=`Unique Pageviews`, colour=`Traffic Source`)) + geom_line() + 
  facet_grid(pagePath ~., scales = "fixed") + theme(legend.position="bottom")

head(May1)
#other
ga_source.df <- aggregate(ga_source$uniquePageviews ~ ga_source$pagePath + ga_source$source 
                          + ga_source$month + ga_source$day + ga_source$dayOfWeek, data=ga_source, sum)
colnames(ga_source.df) <- c("pagePath","source","month", "day", "dayOfWeek", "uniquePageviews")
ga_source.df <- ga_source.df[order(-ga_source.df$uniquePageviews),]

ga_monday <- ga_source.df[which(ga_source.df$dayOfWeek == 1),]

others <- which((ga_source.df$source!="facebook.com"&ga_source.df$source!="twitter.com"
                 &ga_source.df$source!="google.com"&ga_source.df$source!="(direct)"))
ga_source.df$source[others]<-"other"

ga_source.df <- aggregate(ga_source.df$sessions ~ ga_source.df$source + ga_source.df$month , data=ga_source.df, sum)
colnames(ga_source.df) <- c("source","month", "sessions")
