
library(shiny)


shinyUI(navbarPage("Data Products Project",

                   tabPanel("Sentiment Analysis Application",


shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Sentiment Analysis of Twitter Content"),
  
  sidebarPanel(
    
    h3('Input topic to be analyzed: '),
    textInput('user_input_value', NULL, value = "Cats", width = NULL),
    submitButton("ANALYZE")
  ),
  mainPanel(
    h3('Results of Analysis'),
    verbatimTextOutput("inputValue"),
    h3('Histogram'),    
    plotOutput('sentimentHist'),
    h3('Table of results'),
    dataTableOutput( "tableOutputID" )
    
  ))
  
)),
tabPanel("User Documentation"),

titlePanel("User Documentation"),
sidebarLayout(
  sidebarPanel("Sentiment Analysis "),
  mainPanel(
    p("The ", strong("Sentiment Analysis Application"), "gauges the current sentiment of any user inputted topic."),
    p("Simply enter in the keyword you would like to discover the current sentiment for in the", code("'input topic to be analyzed:'"), "text box and click the ", code("'ANALYZE'"), "button", style = "font-family: 'times'; font-si16pt"),
    p("The ", strong("Sentiment Analysis Application "), " internally uses the Twitter API to gather the current tweets on the input topic you entered. It then parses each tweet and
      identifies known positive and negative keywords within the tweet. A score is calculated based on the number and strenghth of the matched words."), 
    p("The ", strong("Sentiment Analysis Application"), "then outputs a ", code("Histogram"), "and a ", code("Table of Results"), "to the main panel. The application plots the scores
      in a histogram, calculates the mean score, identifies it with a red line, and displays the raw tweet data to the user"),
    p("This application was adapted from and credits the tutorials and open source code from", a("http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment"), 
      "and the Sentiment analysis adapted from Hu and Liu's 'opinion lexicon' which categorizes nearly 6,800 words as positive or negative and can be downloaded from 
      Bing Liu's web site:", a("http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar")   )
  ))

))