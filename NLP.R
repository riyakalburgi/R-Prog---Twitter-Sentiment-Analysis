library("shiny")
library("SnowballC")
library("twitteR")
library("syuzhet")

sidebar_content <- sidebarPanel(
  textInput("y_var",label = "Y Variable"),
  actionButton("update", "Change")
)

main_content <- mainPanel(
  plotOutput("plot")
)

panel <- tabPanel(
  "Visualization",
  titlePanel("Whose sentiment analysis you want to do?"),
  p("Please enter twitter user_id."),
  sidebarLayout(
    sidebar_content, main_content
  )
)



# User Interface -----------------------------------------------------
ui <- navbarPage(
  "Sentiment analysis",
  panel
)

#--------------------------------------------------------------------------------------------------------


server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  name <- ""
  observeEvent(input$update, {
    
    consumer_key <- 'dFQd5EoJbywMHBCEax8PTdwVu'
    consumer_secret <- 'zBTK3jJlKzP6fQ7ZO0fxKnURldwHgz08TsgwYdTmOfClOdJMvz'
    access_token <- '1380533717081280512-OGSMrTfVmpiC4MHoJ71slV8WPKtJkB'
    access_secret <- 'vXDAdM2EnEQ3HT5zWx8pAdUnmbne6DvwFclyge5O4mt7r'
    
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    name <- input$y_var
    tweets <- userTimeline(name, n=200)
    
    n.tweet <- length(tweets)
    
    tweets.df <- twListToDF(tweets) 
    
    tweets.df2 <- gsub("/[^\r\n\t\f\v \u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]*http[^\r\n\t\f\v \u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]*[\r\n\t\f\v \u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]?/g","",tweets.df$text)
    
    tweets.df2 <- gsub("/#[^\r\n\t\f\v \u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]*[\r\n\t\f\v \u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]?/g","",tweets.df2)
    
    tweets.df2 <- gsub("/[^\r\n\t\f\v \u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]*@[^\r\n\t\f\v \u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]*[\r\n\t\f\v \u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]?/g","",tweets.df2)
    
    word.df <- as.vector(tweets.df2)
    
    emotion.df <- get_nrc_sentiment(word.df)
    
    emotion.df2 <- cbind(tweets.df2, emotion.df) 
    
    sent.value <- get_sentiment(word.df)
    
    most.positive <- word.df[sent.value == max(sent.value)]
    
    most.negative <- word.df[sent.value <= min(sent.value)] 
    
    positive.tweets <- word.df[sent.value > 0]
    
    negative.tweets <- word.df[sent.value < 0]
    
    neutral.tweets <- word.df[sent.value == 0]
    
    category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
    
    output$plot <- renderPlot({
      barplot(
        sort(colSums(prop.table(emotion.df[, 1:8]))), 
        horiz = TRUE, 
        cex.names = 0.7, 
        las = 1, 
        main = "Emotions in Sample text", xlab="Percentage"
      )
    })
  })
}

shinyApp(ui,server)