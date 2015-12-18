
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

options(httr_oauth_cache=T)
authenticate_twitterAPI()

shinyServer(
  
  function(input, output, session){
    
    obs <- observe({    
      
      # retrieve the tweets associated with the user's input 
      get_tweets( as.character(input$user_input_value), 200 )
      
      # calculate the sentiment of the tweets retrieved
      #reactive( { calculate_sentiment() } )  
      calculate_sentiment()          
      
      # store and send back to ui the sentiment table generated
      output$tableOutputID <- renderDataTable( { as.data.frame(twEnv$sentiment_scores) } )      
      
      #output$prediction <- renderPrint({paste(input$user_telemetry_value, twEnv$sentiment_scores$score) })
      output$sentimentHist <- renderPlot({
        hist( twEnv$sentiment_scores$score, 
              xlab='Sentiment Score', 
              col='lightblue', 
              main = paste("Sentiment Analysis of ", input$user_input_value) )
        m <- mean( twEnv$sentiment_scores$score )
        lines(c(m, m), c(0, 200),col="red",lwd=5)
        text(40, 75, paste("mean = ", m))
        
      })      
    })
    
    # When the client ends the session, suspend the observer
    session$onSessionEnded(function() {
      obs$suspend()
    })     
  
   
    # store and send back to ui the user's original input value 
    output$inputValue <- renderPrint({
      input$user_input_value
    })     
    

    
    
  })
