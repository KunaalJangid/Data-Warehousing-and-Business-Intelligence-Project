install.packages("SnowballC")
install.packages("tm")
install.packages("twitteR")
install.packages("syuzhet")
install.packages(readxl)
install.packages("plyr")
install.packages("reshape")
#Importing the Libraries
library(SnowballC)
library(tm)
library(twitteR)
library(syuzhet)
library(readxl)
library(data.table)
library(plyr)
library(reshape)
library(caret)
#Importing Datasets
pewDiePieSentimentScore <- read.csv("PewdiepieTweets.csv")
tSeriesSentimentScore <- read.csv("TseriesTweets.csv")
fiveMinSentimentScore <- read.csv("FiveminuteTweets.csv")
mostSubscribedChannels <- read_excel("Youtube-most-subscribed-channels-2019.xlsx", sheet=2)
mostViewedChannels <- read_excel("Youtube-most-viewed-channels-2019.xlsx", sheet=2)
highestPaidStars <- read_excel("Highest-paid-youtube-stars-2018.xlsx", sheet=2)
kaggleYoutubeData <- read.csv("channels.csv")

#Cleaning the Data
#Pewdiepie Score
tweets.df <- gsub("http.*","",pewDiePieSentimentScore$text)
tweets.df <- gsub("https.*","",tweets.df)
tweets.df <- gsub("#.*","",tweets.df)

options(scipen=999)
word.df <- as.vector(tweets.df)

sent_value <- get_sentiment(word.df)

pewDiePieSentimentScore$title <- "PewDiePie"

#Removing unnecessary columns from dataframe
pewDiePieSentimentScore <- within(pewDiePieSentimentScore, rm(
  favorited, favoriteCount, replyToSN, truncated, replyToSID,
  replyToUID, statusSource, screenName,
  retweetCount, isRetweet, retweeted, longitude, latitude, created, text, X))

PewdiepieTwitterSentiments <- cbind(pewDiePieSentimentScore, sent_value)

#Tseries Score

tweets.df2 <- gsub("http.*","",tSeriesSentimentScore$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)

word.df2 <- as.vector(tweets.df2)

sent_value <- get_sentiment(word.df2)

tSeriesSentimentScore$title <- "T-Series"

tSeriesSentimentScore <- within(tSeriesSentimentScore, rm(
  favorited, favoriteCount, replyToSN, truncated, replyToSID,
  replyToUID, statusSource, screenName,
  retweetCount, isRetweet, retweeted, longitude, latitude, created, text, X))

TseriesTwitterSentiments <- cbind(tSeriesSentimentScore, sent_value)

#5-Minute Crafts Score

tweets.df3 <- gsub("http.*","",fiveMinSentimentScore$text)
tweets.df3 <- gsub("https.*","",tweets.df3)
tweets.df3 <- gsub("#.*","",tweets.df3)

word.df3 <- as.vector(tweets.df3)

sent_value <- get_sentiment(word.df3)

fiveMinSentimentScore$title <- "5-Minute Crafts"

fiveMinSentimentScore <- within(fiveMinSentimentScore, rm(
  favorited, favoriteCount, replyToSN, truncated, replyToSID,
  replyToUID, statusSource, screenName,
  retweetCount, isRetweet, retweeted, longitude, latitude, created, text, X))

FiveMinuteTwitterSentiments <- cbind(fiveMinSentimentScore, sent_value)

#Combining the Dataframes
twitterSentiments <- rbind(PewdiepieTwitterSentiments, TseriesTwitterSentiments, FiveMinuteTwitterSentiments)

sentiment = c()
for(i in 1: nrow(twitterSentiments))
{
 if(twitterSentiments$sent_value[i]>0)
  {
    sentiment[i] = "Positive"
  }
  else if(twitterSentiments$sent_value[i]<0)
  {
    sentiment[i] = "Negative"
  }
  else
  {
    sentiment[i] = "Neutral"
  }
}
twitterSentiments <- cbind(twitterSentiments, sentiment)

tweets_summary = twitterSentiments %>% group_by(title,sentiment) %>% summarise(Freq=n())
tweets_summary = cast(tweets_summary, title ~ sentiment)

write.csv(tweets_summary, "TwitterSentiments.csv", row.names=FALSE)

#Cleaning Most Subscribed Channels Dataframe
mostSubscribedChannels <- mostSubscribedChannels[-c(13:14),]
mostSubscribedChannels <- mostSubscribedChannels[-c(1:2),]
names(mostSubscribedChannels)= c("title", "subscribers")
write.csv(mostSubscribedChannels, "Cleaned_Most_Subscribed_Channels.csv", row.names = FALSE)

#Cleaning Highest Paid Stars
highestPaidStars <- highestPaidStars[-c(13:14),]
highestPaidStars <- highestPaidStars[-c(1:2),]
names(highestPaidStars)= c("title", "income")
write.csv(highestPaidStars, "Cleaned_Highest_Paid_Stars.csv", row.names = FALSE)

#Cleaning Most Viewed Channels Dataframe
mostViewedChannels <- mostViewedChannels[-c(13:14),]
mostViewedChannels <- mostViewedChannels[-c(1:2),]
names(mostViewedChannels)= c("title", "views")
write.csv(mostViewedChannels, "Cleaned_Most_Viewed_Channels.csv", row.names = FALSE)

#Cleaning Kaggle Youtube Data

kaggleYoutubeData = fread("channels.csv", encoding = "Latin-1")
kaggleYoutubeData <- kaggleYoutubeData[,-c(8, 9, 10, 12, 13)]
kaggleYoutubeData = kaggleYoutubeData[c(1:1000),]
write.csv(kaggleYoutubeData, "Cleaned_Kaggle_Data.csv", row.names = FALSE)

noise_rows =(kaggleYoutubeData$title) %like% "%<U+%"

