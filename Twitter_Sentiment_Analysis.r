#A machine learning package for automatic text classification that makes it simple for novice #users to get started with machine learning, while allowing experienced users to easily #experiment with different settings and algorithm combinations.
>library(RTextTools)

#R supports a package called ‘e1071’ which provides the naive bayes training function.
>library(e1071) 

#The caret package contains functions to streamline the model training process for complex #regression and classification problems.
>library(caret) 

#word cloud generator package helping us to analyze texts and to quickly visualize the #keywords as a word cloud.
>library(wordcloud)

>set.seed(12345)

#Given dataset of positive sentiment tweets
>sentPositive <- c(
  "I like it", "like it a lot", "It's really good",
  "recommend!", "Enjoyed!", "like it",
  "It's really good", "recommend too",
  "outstanding", "good", "recommend!",
  "like it a lot", "really good", 
  "Definitely recommend!", "It is fun",
  "liked!", "highly recommend this",
  "fantastic show", "exciting",
  "Very good", "it's ok",
  "exciting show", "amazing performance",
  "it is great!","I am excited a lot",
  "it is terrific", "Definitely good one",
  "very satisfied", "Glad we went",
  "Once again outstanding!", "awesome"
)

# dataset of negative sentiment tweets
>sentNegative <- c(
  "Not good at all!", "rude",
  "It is rude", "I don't like this type",
  "poor", "Boring", "Not good!",
  "not liked", "I hate this type of",
  "not recommend", "not satisfied",
  "not enjoyed", "Not recommend this.",
  "disgusting movie","waste of time",
  "feel tired after watching this",
  "horrible performance", "not so good",
  "so boring I fell asleep", "poor show",
  "a bit strange","terrible"
)

#Generating keywords from dataset
>wordcloud(sentPositive, max.words = 40, scale = c(3, 0.5))

>wordcloud(sentNegative, max.words = 40, scale = c(3, 0.5))

#creating a single dataframe to hold both positive and negative tweets
>df = data.frame(sentiment="positive", text=sentPositive)
>df = rbind(df, data.frame(sentiment="negative", text=sentNegative))

# creating a sample of ramdow tweets from df to split data in testing and training set
>index = sample(1:nrow(df), size = .9 * nrow(df))

# 90% of data from dataset used for training
>train = df[index, ]

#remaining 10% which is not included in train set is used for testing 
>test = df[-index, ]

>head(train)
>head(test)

>mTrain = create_matrix(train[,2], language = "english", 
                       removeStopwords=FALSE, removeNumbers=TRUE, 
                       stemWords=FALSE) 
>matTrain = as.matrix(mTrain)

>mTest = create_matrix(test[,2], language = "english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 

>matTest = as.matrix(mTest)

>labelTrain = as.factor(train[,1])
>labelTest = as.factor(test[,1])

>model = naiveBayes(matTrain, labelTrain) 

>pred = predict(model, matTrain) 

confusionMatrix(labelTrain, pred)

>pred = predict(model, matTest)
>data.frame(test, pred)

>confusionMatrix(labelTest, pred)


#shiny package provides GUI support to R project
>library("shiny")

#An R interface to the C 'libstemmer' library that implements Porter's word stemming algorithm for collapsing words to a common root to aid comparison of vocabulary
>library("SnowballC")

#Provides an interface to the Twitter web API.
>library("twitteR")

#The package comes with four sentiment dictionaries and provides a method for accessing #the robust, but computationally expensive, sentiment extraction tool developed in the NLP #group at Stanford.
>library("syuzhet")

>sidebar_content <- sidebarPanel(
  textInput("y_var",label = "user_id"),
  actionButton("update", "Change")
)

>main_content <- mainPanel(
  plotOutput("plot")
)

>panel <- tabPanel(
  "Visualization",
  titlePanel("Whose sentiment analysis you want to do?"),
  p("Please enter twitter user_id."),
  sidebarLayout(
    sidebar_content, main_content
  )
)

# User Interface
>ui <- navbarPage(
  "Sentiment analysis",
  panel
)


>server <- function(input, output, session) {
  name <- ""

# Update data and rerun app only if change button is clicked
  observeEvent(input$update, {
    
    #credentials to access Twitter API
    consumer_key <- 'dFQd5EoJbywMHBCEax8PTdwVu'
    consumer_secret <- 'zBTK3jJlKzP6fQ7ZO0fxKnURldwHgz08TsgwYdTmOfClOdJMvz'
    access_token <- '1380533717081280512-OGSMrTfVmpiC4MHoJ71slV8WPKtJkB'
    access_secret <- 'vXDAdM2EnEQ3HT5zWx8pAdUnmbne6DvwFclyge5O4mt7r'
    
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    name <- input$y_var

   #fetch latest 200 tweets of given twitter user
    tweets <- userTimeline(name, n=200)
 

    n.tweet <- length(tweets)
    tweets.df <- twListToDF(tweets) 

    # Cleaning the dataset
    tweets.df2 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text);
    tweets.df2 = gsub("@\\w+", "", tweets.df2); # regex for removing @user
    tweets.df2 = gsub("[[:punct:]]", "", tweets.df2); # regex for removing punctuation     mark
    tweets.df2 = gsub("[[:digit:]]", "", tweets.df2); # regex for removing numbers
    tweets.df2 = gsub("http\\w+", "", tweets.df2);# regex for removing links
    tweets.df2 = gsub("#", " ", tweets.df2);
    tweets.df2 = gsub("\n", " ", tweets.df2);  ## regex for removing new line (\n)
    tweets.df2 = gsub("[ \t]{2,}", " ", tweets.df2); ## regex for removing two blank space
    tweets.df2 =  gsub("[^[:alnum:]///' ]", " ", tweets.df2)     # keep only alpha numeric 
    tweets.df2 =  iconv(tweets.df2, "latin1", "ASCII", sub="")   # Keep only ASCII characters
    tweets.df2 = gsub("^\\s+|\\s+$", "", tweets.df2);
 

    word.df <- as.vector(tweets.df2)     
   emotion.df <- get_nrc_sentiment(word.df)

 
    
    sent.value <- get_sentiment(word.df)

 
    most.positive <- word.df[sent.value == max(sent.value)]

 
    
    positive.tweets <- word.df[sent.value > 0]
 

    most.negative <- word.df[sent.value <= min(sent.value)] 

 
    
    negative.tweets <- word.df[sent.value < 0]

 
    
    neutral.tweets <- word.df[sent.value == 0]


category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive",    "Neutral"))

    head(category_senti)

    table(category_senti)

 
# Finally creating a plot on the percentage of sentiments analyzed.
    output$plot <- renderPlot({
      barplot(
        sort(colSums(prop.table(emotion.df[, 1:8]))), 
        horiz = TRUE, 
        cex.names = 0.7, 
        las = 1, 
        main = " Sentiments in Sample text", xlab="Percentage"
      )
    })
  })
}
 

# Command to run shinyApp
shinyApp(ui,server)
