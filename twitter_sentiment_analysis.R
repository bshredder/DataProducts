#########################################################################################################################
#
# Title:        Twitter Sentiment Analysis
# Description:  Gauges sentiment of topic by reading tweets with related keywords and then performing 
#               a sentiment analysis on the list of tweets
#
# Author:   Bill Schroeder
# Credits:  Tutorial and source adapted from http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment
#           Sentiment analysis adapted from Hu and Liu's "opinion lexicon" which categorizes nearly 6,800 words as 
#           positive or negative and can be downloaded from Bing Liu's web site: 
#           http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar
#
#########################################################################################################################

#
# required libraries
#

library (plyr)
library (stringr)
library(httr)
library(twitteR)
library(ROAuth)
library(ggplot2)

#
# create new environment and initialize evil global variables
#

twEnv = new.env()
twEnv$tweet_list <- character()
twEnv$tweet_df <- data.frame()

twEnv$sentiment_scores = data.frame()
twEnv$sentiment_neg_scores = data.frame()
twEnv$sentiment_pos_scores = data.frame()

# load "standard" sentiment list from http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar

twEnv$sentiment_list_pos = scan('positive-words.txt', what='character', comment.char=';')
twEnv$sentiment_list_neg = scan('negative-words.txt', what='character', comment.char=';')


#
# FUNCTIONS
#

heartbeat_test <- function(){
  print("heartbeat")
}

run <- function( wordMatches, numTweets ){

  authenticate_twitterAPI()
  get_tweets( wordMatches, numTweets )
  calculate_sentiment()
  output_results()
  
}



#
# Synopsis: function that authenticates this session with the twitter api
#           TODO - remove setup_twitter_oauth so that client tokens/secrets 
#           are not publicly visible
#

authenticate_twitterAPI <- function( ){
  
  setup_twitter_oauth("",
                      "",
                      "",
                      "")
}


#
# Synopsis: function appends a new sentiment word to correct sentiment word list
#           Either positive or negative list is updated with new word - or an error is
#           thrown when word is incorrectly classified
#

append_sentiment_list <- function( newWord, sentimentCategory ){
    
  if( sentimentCategory == 'positive'){
    twEnv$sentiment_list_pos <- c( twEnv$sentiment_list_pos, newWord )
  }
  else if(sentimentCategory == 'negative'){
    twEnv$sentiment_list_neg <- c( twEnv$sentiment_list_neg, newWord )
  }
  else{
    print("WARNING: incorrect sentiment category")
  }
}

#
# Synopsis: function gets the tweets associated with the user input. 
#           User enters text and number of tweets to retrieve. Function uses
#           Twitter API to retrieve associated tweets
#

get_tweets <- function( search_word, num_samples = 200 ){
  
  twEnv$tweet_list <- searchTwitter( search_word, num_samples )  
  twEnv$tweet_df <- twListToDF( twEnv$tweet_list )  

  #convert text to factor
  twEnv$tweet_df$text <- as.factor( twEnv$tweet_df$text )  
}



#
# Synopsis: function outputs the results of the sentiment analsysis for the user selected tweets
#

output_results <- function() { 

  hist( twEnv$sentiment_scores$score )
  table( twEnv$sentiment_scores$score )
}

#
# Synopsis: helper function to get around errors thrown in R library's tolower function when it parses emoticons
#           nice writeup on this at http://gastonsanchez.com/blog/how-to/2012/05/29/Catching-errors-when-using-tolower.html
#

tryTolower = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}


#
# Synopsis: Function calculates a sentiment score of a tweet by matching posisive and negative words in the tweet text
#           Design and code snippets are credited to author jeffreybreen from his Tutorial and source @
#           http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment
#           


calculate_sentiment = function( sentences = twEnv$tweet_df$text,
                            pos.words = twEnv$sentiment_list_pos,
                            neg.words = twEnv$sentiment_list_neg,
                            .progress='none')  {  
  
  good.smiley <- c(":)")
  bad.smiley <- c(":(",";)",":'",":P") 
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    
    # clean up sentences with R's regex-driven global substitute, gsub():  
    sentence = gsub(":)", 'awsum', sentence)
    sentence = gsub('[[:punct:]]', '', sentence)  
    sentence = gsub('[[:cntrl:]]', '', sentence)  
    sentence = gsub('\\d+', '', sentence)  
    
    # and convert to lower case:  
    sentence = tryTolower(sentence)  
    
    # split into words. str_split is in the stringr package  
    word.list = str_split(sentence, '\\s+')  
    
    # sometimes a list() is one level of hierarchy too much  
    words = unlist(word.list)  
    
    # compare our words to the dictionaries of positive & negative terms  
    pos.matches = match(words, twEnv$sentiment_list_pos)  
    neg.matches = match(words, twEnv$sentiment_list_neg)  
    
    # match() returns the position of the matched term or NA  
    # we just want a TRUE/FALSE:  
    pos.matches = !is.na(pos.matches)  
    neg.matches = !is.na(neg.matches)  
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():  
    score = sum(pos.matches) - sum(neg.matches)  
    return(score)  
    
  }, pos.words, neg.words, .progress=.progress ) 
  
  scores.df = data.frame(score=scores, text=sentences)  
  twEnv$sentiment_scores <- scores.df
  twEnv$sentiment_neg_scores = subset(twEnv$sentiment_scores, twEnv$sentiment_scores$score < 0)
  twEnv$sentiment_pos_scores = subset(twEnv$sentiment_scores, twEnv$sentiment_scores$score > 0)  
  
  return(scores.df)
} 
