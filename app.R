library(shiny)
library(plyr)
library(dplyr)
library(rsconnect)

hotelRatings <- read.csv("CleanHotelRatings.csv")
hotelRatings$Pool <- revalue(hotelRatings$Pool, c("YES"=1))
hotelRatings$Pool <- revalue(hotelRatings$Pool, c("NO"=0))
hotelRatings$Gym <- revalue(hotelRatings$Gym, c("YES"=1))
hotelRatings$Gym <- revalue(hotelRatings$Gym, c("NO"=0))
hotelRatings$TennisCourt <- revalue(hotelRatings$TennisCourt, c("YES"=1))
hotelRatings$TennisCourt <- revalue(hotelRatings$TennisCourt, c("NO"=0))
hotelRatings$Spa <- revalue(hotelRatings$Spa, c("YES"=1))
hotelRatings$Spa <- revalue(hotelRatings$Spa, c("NO"=0))
hotelRatings$Casino <- revalue(hotelRatings$Casino, c("YES"=1))
hotelRatings$Casino <- revalue(hotelRatings$Casino, c("NO"=0))
hotelRatings$FreeInternet <- revalue(hotelRatings$FreeInternet, c("YES"=1))
hotelRatings$FreeInternet <- revalue(hotelRatings$FreeInternet, c("NO"=0))
hotelRatings$Pool <- as.numeric(hotelRatings$Pool)
hotelRatings$Gym <- as.numeric(hotelRatings$Gym)
hotelRatings$TennisCourt <- as.numeric(hotelRatings$TennisCourt)
hotelRatings$Spa <- as.numeric(hotelRatings$Spa)
hotelRatings$Casino <- as.numeric(hotelRatings$Casino)
hotelRatings$FreeInternet <- as.numeric(hotelRatings$FreeInternet)

# Define UI ----
ui <- fluidPage(
  titlePanel("Las Vegas Hotel Ratings"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create logistic regression graphs with 
               information from the Las Vegas Strip Data Set."),
      
      selectInput("var", 
                  label = "Y-Val",
                  choices = c("Pool", 
                              "Gym",
                              "Tennis Court", 
                              "Spa",
                              "Casino",
                              "Free Internet"),
                  selected = "Pool"),
      
      radioButtons("check",
                         label = "Select a Rating",
                         choices = c("1", '2', '3', '4', '5'),
                         selected = '5'
      ),
      
    ),
    

    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", verbatimTextOutput("plot"), 
                           plotOutput("realPlot"),
                           plotOutput("histogram")),
                  tabPanel("About", verbatimTextOutput("about"))
      )
      
    )
    
    
  )
)


# Define server logic ----
server <- function(input, output) {
  output$plot <- renderText({
    yvals <- switch(input$var, 
                    "Pool" = hotelRatings$Pool,
                    "Gym" = hotelRatings$Gym,
                    "Tennis Court" = hotelRatings$TennisCourt,
                    "Spa" = hotelRatings$Spa,
                    "Casino" = hotelRatings$Casino,
                    "Free Internet" = hotelRatings$FreeInternet)
    rateval <- input$check
    logistic <- glm(yvals ~ Score, data=hotelRatings, family="binomial")
    newData <- data.frame(Score=as.numeric(rateval))
    prediction <- predict(logistic, newData, type='response')
    paste("The probability that selected Y-value will occur based
          on selected rating is", prediction)
    


  })
  
  output$realPlot <- renderPlot({
    yvals2 <- switch(input$var, 
                    "Pool" = hotelRatings$Pool,
                    "Gym" = hotelRatings$Gym,
                    "Tennis Court" = hotelRatings$TennisCourt,
                    "Spa" = hotelRatings$Spa,
                    "Casino" = hotelRatings$Casino,
                    "Free Internet" = hotelRatings$FreeInternet)

    xweight <- seq(0, 5, 1.00)
    
    logistic2 <- glm(yvals2 ~ Score, data=hotelRatings, family="binomial")
    allRates <- data.frame(Score=hotelRatings$Score)
    prediction2 <- predict(logistic2, list(Score=xweight), type='response')
    plot(hotelRatings$Score, hotelRatings$Pool, pch = 16, xlab = "Rating", ylab = "Y-Val")
    lines(xweight, prediction2)
  })
  
  output$histogram <- renderPlot({
    onlyYespool <- subset(hotelRatings, Pool == 1)
    onlyYesgym <- subset(hotelRatings, Gym == 1)
    onlyYestennis <- subset(hotelRatings, TennisCourt == 1)
    onlyYesspa <- subset(hotelRatings, Spa == 1)
    onlyYescasino <- subset(hotelRatings, Casino == 1)
    onlyYesfreewifi <- subset(hotelRatings, FreeInternet == 1)

    barsToPlot <- matrix(c(nrow(onlyYespool), nrow(onlyYesgym), nrow(onlyYestennis),
             nrow(onlyYesspa), nrow(onlyYescasino), nrow(onlyYesfreewifi)),
             ncol=6, dimnames=list("frequency",c("pool","gym","tennis", "spa", "casino", 
                                     "free internet")))
    barplot(barsToPlot, ylap=list("Frecuency"), main="")
    title(main=list("Number of times these variables are in a hotel"))

  })
  
  output$about <- renderText({
    paste(" For this case study, I collected data on Las Vegas hotels to determine
    how important certain aspects of a hotel are to people staying in them. This topic
    is interesting to me because Las Vegas is a very social city, with many people 
    coming on trips, many times to hit the casinos. I think that one important part
    of someone's trip is how comfortable the place they are staying at is. If people
    enjoy their hotel, they are more likely to come back and stay at the same place.
    I like visiting cities, and think that it is important for these places to provide
    a nice place for people to stay and bet all of their money away.
    
    
    Following the data science lifecycle, I first started in the discovery 
          phase and determined the general analytic problem to be regression. The 
          analytics problem to solve is: what aspects of hotels (specifically in
          Las Vegas) do people like the most? This is an important question for 
          people who are interested in building hotels or making additions to hotels in Las Vegas. If the hotel
          contains aspects that people like, then they are more likely to stay at
          the hotel, meaning that the hotel will make more money. My initial hypothesis
          is that hotels with free internet and casinos have higher ratings than 
          hotels with a gym, tennis court, spa, and pool. During the data
          preparation phase, I gathered data from the UCI machine learning repisitory
          and used the Las Vegas Strip Data Set. This data set includes reviews of 21
          different hotels located in Las Vegas Strip. From there I changed all of the variables
          using the strings Yes and No and changed them to the binary numbers 1 and 0. 
          Including these variables, I also used the variable Score which is the rating
          (on a scale of 1-5) that the reviewer gave the hotel. During the model planning
          phase I chose to use logistic regression to identify the most influential
          factors in the rating of a hotel. Next into model building, I used the glm() 
          function in R to build my model in order to look at the probability that one of 
          the aspects of a hotel is present in a certain rating. For example, if the user
          specifies pool and rating of 5, my model will show the probability that a hotel 
          has a pool if the review of a hotel is a 5 star review. My results showed that
          hotels with good reviews are more likely to have a pool or free internet.
          On the other hand, a hotel recieving a good review does not affect the probability
          that the hotel has a tennis court, spa, or gym. Interestingly, my model shows that
          a good review actually has less of a chance of having a casino than a bad review.
          However, the probability is still 90%.
          
          Details of logistic regression algorithm:
          During logistic regression, we are taking the natural log of an event and dividing
          it by the event not occurring. This is because we are interested in the 
          probability that the event occurs P(Y=1)).Using the logit on this probability, we 
          are able to do logistic regression. This algorithm is iterative because it
          reweighs least squares. The outcome of a logistic regression is to estimate the 
          probability that an event will occur as a function of other variables and it is 
          a linear expression for predicting the log-odds ratio of an outcome. The log-odds
          is used because it is easily converted to a regular probability.
          
          ")
  })

  
}

# Run app ----
shinyApp(ui, server)
