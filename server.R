
library(shiny)
library (plyr)
library (stringr)
library(httr)
library(twitteR)
library(ROAuth)
library(ggplot2)
library(base64enc)

# share across all sessions
source('twitter_sentiment_analysis.R', local=TRUE)

authenticate_twitterAPI()
sentiment_list_pos = scan('positive-words.txt', what='character', comment.char=';')
sentiment_list_neg = scan('negative-words.txt', what='character', comment.char=';')

shinyServer(
  
  function(input, output, session){

    obs <- observe({      

      # cat( "Enter observe()", input$user_input_value )

      if(input$user_input_value != ""){
        
        # retrieve the tweets associated with the user's input 
        shiny_tweetList <- get_tweets( input$user_input_value, 20 )
        
        cat( "shiny_tweetList", length(shiny_tweetList$text) )            
        if( length(shiny_tweetList$text) > 0){
          
          # calculate the sentiment of the tweets retrieved
          shiny_sentiment_scores <- calculate_sentiment(shiny_tweetList$text, sentiment_list_pos, sentiment_list_neg)          
          
          output$sentimentHist <- renderPlot({
      
            validate(
              need(shiny_sentiment_scores$score, 'Sorry ;-( no sentiment scores could be calculated')
            )      
            
            ggplot(data = shiny_sentiment_scores, aes( score ) ) +
              geom_histogram(breaks=seq(-6,6,by= 1), col="red", fill="green", alpha = .2) + 
              geom_vline( xintercept = mean( shiny_sentiment_scores$score), size = 1.5, color="black", linetype = "longdash") +
              labs(title=paste("Sentiment Analysis Histogram of ", input$user_input_value )) +
              labs(x="sentiment Score", y="Count") 
          })
          
          # store and send back to ui the sentiment table generated
          output$tableOutputID <- renderDataTable( { as.data.frame( shiny_sentiment_scores ) } )   
        }
      }
      else{
        rm(shiny_tweetList)
        rm(shiny_sentiment_scores)
      }
      
      print( "Exit observe()" )
    })
    
    # store and send back to ui the user's original input value 
    output$inputValue <- renderPrint({
      input$user_input_value
    })      
 
  })
