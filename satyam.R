library(twitteR)
library(purrr)
library(dplyr)
require('ROAuth')
require('RCurl')
library(plyr)
library(stringr)
## SENTIMENT FUNCTION
## NEW
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress =.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}
##NEW

pos.words= scan('C:/Users/satyam/Downloads/Sentiment Analysis/positive-words.txt', what='character', comment.char=';')
neg.words= scan( 'C:/Users/satyam/Downloads/Sentiment Analysis/negative-words.txt', what='character', comment.char=';')
bscore <- score.sentiment(tweet_df$text,pos.words,neg.words,.progress = 'text')
rscore <- score.sentiment(tweet2_df$text,pos.words,neg.words,.progress = 'text')
hist(rscore$score)
hist(bscore$score)
consumer_key <- "e2viTtGxg5HcNMIxMnZ0Ya2n8"
consumer_secret <- "Ql1Z8NdEiZ3lIO0tkiaWegYJ68iNEyE5T9ZNdqIb8u8X2vJCI2"
access_token <- "1576630231-Z99KiV4Eu4XDkiE8dIU7nAIJ0VS3tldUcLJ2OoD"
access_secret <- "CA12U5jFD1pqrFrwrbuxroN4dJCpanQC7hsweEI3H0lz7"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweet1 <- userTimeline("@barcelona",n=150)
tweet2<- userTimeline("realmadrid",n=150)
tweet_df <-tbl_df(map_df(tweet1,as.data.frame))
tweet2_df <- tbl_df(map_df(tweet2,as.data.frame))




  
  
                         
  
                          
    
                  
  
    
  
    
  
