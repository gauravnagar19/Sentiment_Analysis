install.packages("twitteR")
install.packages("sentimentr")
install.packages("plyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")

install.packages("ROAuth")

library(twitteR)
library(sentimentr)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(ROAuth)


dir.create(file.path("/TwitterFeedAnalysis", "#formeplusyouonspotify"), mode = "0777")
dir.create(file.path("/TwitterFeedAnalysis", "#RogueOne"), mode = "0777")

setwd("/resources/RogueOne")



search_term <- "#formeplusyouonspotify, #formeplusyou"

search_term <- unlist(strsplit(search_term,","))

tweets = list()

## You may get warning because of Twitter rate limit.
## If there is many hashtags, then you may need to stop it after sometime since
## Twitter will impose the Rate Limit and you will be getting the exception to 
## getting the tweets. 
for(i in 1:length(search_term)){
  result<-searchTwitter(search_term[i],n=1000,lang="en")
  tweets <- c(tweets,result)
  tweets <- unique(tweets)
}


length(tweets)

head(tweets)


file<-NULL

if (file.exists("tweetsformeplusyou.csv")){file<- read.csv("tweetsformeplusyou.csv")}

df <- do.call("rbind", lapply(tweets, as.data.frame))

df<-rbind(df,file)

df <- df[!duplicated(df[c("id")]),]

write.csv(df,file="tweetsformeplusyou.csv",row.names=FALSE)





library(NLP)
library(tm)

twitter_formeplusyou_df = twListToDF(tweets)

r_text_corpus <- Corpus(VectorSource(twitter_formeplusyou_df$text))

r_text_cleansing <- tm_map(r_text_corpus, stripWhitespace)

r_text_cleansing <- tm_map(r_text_cleansing, removeNumbers)

r_text_cleansing <- tm_map(r_text_cleansing, removePunctuation)

r_text_cleansing <- tm_map(r_text_cleansing, content_transformer(tolower))



install.packages("syuzhet")

library(syuzhet)

isNull <- function(data) {
  if(is.null(data))
    return(0)
  else
    return(data)
}

text_vec = c()
anger = c() ; anticipation=c() ; disgust=c() ; fear=c() ; joy=c() ;
sadness=c() ; surprise=c() ; rust=c() ; nrc_negative=c() ; nrc_positive=c();

for(i in 1:length(r_text_cleansing)){
  text <- lapply(r_text_cleansing[i], as.character)
  text <- gsub("http\\w+", "", text)
  nrc_emotions <- get_nrc_sentiment(text)
  
  text_vec[i] <- text
  anger[i] <- isNull(nrc_emotions$anger)
  anticipation[i] <- isNull(nrc_emotions$anticipation)
  disgust[i] <- isNull(nrc_emotions$disgust)
  fear[i] <- isNull(nrc_emotions$fear)
  joy[i] <- isNull(nrc_emotions$joy)
  sadness[i] <- isNull(nrc_emotions$sadness)
  surprise[i] <- isNull(nrc_emotions$surprise)
  rust[i] <- isNull(nrc_emotions$rust)
  nrc_negative[i] <- isNull(nrc_emotions$negative)
  nrc_positive[i] <- isNull(nrc_emotions$positive)
}

nrc_df <- data.frame(text_vec,anger,anticipation,disgust,fear,joy,sadness,surprise,
                     rust,nrc_negative,nrc_positive)

nrc_df[1:6,1:11]

par(mar=c(5.1,5,4.1,2.1))

barplot(
  sort(colSums(prop.table(nrc_df[, 2:9]))), 
  horiz = TRUE, 
  cex.names = 0.7,
  las = 1, 
  main = "Emotions for Album: For Me+You", 
  xlab="Percentage",
  col="lightblue"
)
