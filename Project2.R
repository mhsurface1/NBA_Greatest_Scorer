#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
Kobe <- read.csv("Kobe.csv")
MJ <- read.csv("MJ.csv")
Bron <- read.csv("Bron.csv")
KD <- read.csv("KD.csv")
Kareem <- read.csv("Kareem.csv")
Magic <- read.csv("Magic.csv")
Larry <- read.csv("Larry.csv")
Curry <- read.csv("Curry.csv")
Westbrook <- read.csv("Westbrook.csv")
Karl <- read.csv("Karl.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Who is the Greatest Scorer in NBA History?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("variablechoice", "Choice of Player",
                         choices = c("Kobe Bryant", "Michael Jordan", 
                                     "Lebron James", "Kevin Durant",
                                     "Kareem Abdul-Jabbar", "Magic Johnson", 
                                     "Larry Bird", "Steph Curry",
                                     "Russell Westbrook", "Karl Malone"),
                         selected = "Kobe Bryant")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterplot"),
           textOutput("usefullabel"),
           verbatimTextOutput("summaryofvariable"),
           plotOutput("scatterplot2"),
           textOutput("usefullabel2"),
           verbatimTextOutput("summaryofvariable2"),
           plotOutput("scatterplot3"),
           textOutput("usefullabel3"),
           verbatimTextOutput("summaryofvariable3")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        if (input$variablechoice == "Kobe Bryant") {x <- Kobe[,2]}
        if (input$variablechoice == "Michael Jordan") {x <- MJ[,2]}
        if (input$variablechoice == "Lebron James") {x <- Bron[,2]}
        if (input$variablechoice == "Kevin Durant") {x <- KD[,2]}
        if (input$variablechoice == "Kareem Abdul-Jabbar") {x <- Kareem[,2]}
        if (input$variablechoice == "Magic Johnson") {x <- Magic[,2]}
        if (input$variablechoice == "Larry Bird") {x <- Larry[,2]}
        if (input$variablechoice == "Steph Curry") {x <- Curry[,2]}
        if (input$variablechoice == "Russell Westbrook") {x <- Westbrook[,2]}
        if (input$variablechoice == "Karl Malone") {x <- Karl[,2]}
        
        if (input$variablechoice == "Kobe Bryant") {y <- Kobe[,30]}
        if (input$variablechoice == "Michael Jordan") {y <- MJ[,30]}
        if (input$variablechoice == "Lebron James") {y <- Bron[,30]}
        if (input$variablechoice == "Kevin Durant") {y <- KD[,30]}
        if (input$variablechoice == "Kareem Abdul-Jabbar") {y <- Kareem[,30]}
        if (input$variablechoice == "Magic Johnson") {y <- Magic[,30]}
        if (input$variablechoice == "Larry Bird") {y <- Larry[,30]}
        if (input$variablechoice == "Steph Curry") {y <- Curry[,30]}
        if (input$variablechoice == "Russell Westbrook") {y <- Westbrook[,30]}
        if (input$variablechoice == "Karl Malone") {y <- Karl[,30]}

        # draw the histogram with the specified number of bins
        # hist(x, breaks = seq(0,40,by=1), col = 'darkgray', border = 'white', xlab = "age", main = "Average Points Per Game")
        plot(x, y, xlab = "Age", ylab = "Points Per Game", main = "Average Points Per Game")
    })
    output$summaryofvariable <- renderPrint({
        if (input$variablechoice == "Kobe Bryant") {y <- Kobe[,30]}
        if (input$variablechoice == "Michael Jordan") {y <- MJ[,30]}
        if (input$variablechoice == "Lebron James") {y <- Bron[,30]}
        if (input$variablechoice == "Kevin Durant") {y <- KD[,30]}
        if (input$variablechoice == "Kareem Abdul-Jabbar") {y <- Kareem[,30]}
        if (input$variablechoice == "Magic Johnson") {y <- Magic[,30]}
        if (input$variablechoice == "Larry Bird") {y <- Larry[,30]}
        if (input$variablechoice == "Steph Curry") {y <- Curry[,30]}
        if (input$variablechoice == "Russell Westbrook") {y <- Westbrook[,30]}
        if (input$variablechoice == "Karl Malone") {y <- Karl[,30]}
        
        summary(y)
        
    })
    
    output$usefullabel <- renderText({
        "Summary of Average Points Per Game"
    })
    
    output$scatterplot2 <- renderPlot({
        if (input$variablechoice == "Kobe Bryant") {x <- Kobe[,2]}
        if (input$variablechoice == "Michael Jordan") {x <- MJ[,2]}
        if (input$variablechoice == "Lebron James") {x <- Bron[,2]}
        if (input$variablechoice == "Kevin Durant") {x <- KD[,2]}
        if (input$variablechoice == "Kareem Abdul-Jabbar") {x <- Kareem[,2]}
        if (input$variablechoice == "Magic Johnson") {x <- Magic[,2]}
        if (input$variablechoice == "Larry Bird") {x <- Larry[,2]}
        if (input$variablechoice == "Steph Curry") {x <- Curry[,2]}
        if (input$variablechoice == "Russell Westbrook") {x <- Westbrook[,2]}
        if (input$variablechoice == "Karl Malone") {x <- Karl[,2]}
        
        if (input$variablechoice == "Kobe Bryant") {y <- Kobe[,11]}
        if (input$variablechoice == "Michael Jordan") {y <- MJ[,11]}
        if (input$variablechoice == "Lebron James") {y <- Bron[,11]}
        if (input$variablechoice == "Kevin Durant") {y <- KD[,11]}
        if (input$variablechoice == "Kareem Abdul-Jabbar") {y <- Kareem[,11]}
        if (input$variablechoice == "Magic Johnson") {y <- Magic[,11]}
        if (input$variablechoice == "Larry Bird") {y <- Larry[,11]}
        if (input$variablechoice == "Steph Curry") {y <- Curry[,11]}
        if (input$variablechoice == "Russell Westbrook") {y <- Westbrook[,11]}
        if (input$variablechoice == "Karl Malone") {y <- Karl[,11]}
        
        # draw the histogram with the specified number of bins
        # hist(x, breaks = seq(0,40,by=1), col = 'darkgray', border = 'white', xlab = "age", main = "Average Points Per Game")
        plot(x, y, xlab = "Age", ylab = "Field Goal Percentage", main = "Average Field Goal Percentage")
    })
    
    output$summaryofvariable2 <- renderPrint({
        if (input$variablechoice == "Kobe Bryant") {y <- Kobe[,11]}
        if (input$variablechoice == "Michael Jordan") {y <- MJ[,11]}
        if (input$variablechoice == "Lebron James") {y <- Bron[,11]}
        if (input$variablechoice == "Kevin Durant") {y <- KD[,11]}
        if (input$variablechoice == "Kareem Abdul-Jabbar") {y <- Kareem[,11]}
        if (input$variablechoice == "Magic Johnson") {y <- Magic[,11]}
        if (input$variablechoice == "Larry Bird") {y <- Larry[,11]}
        if (input$variablechoice == "Steph Curry") {y <- Curry[,11]}
        if (input$variablechoice == "Russell Westbrook") {y <- Westbrook[,11]}
        if (input$variablechoice == "Karl Malone") {y <- Karl[,11]}
        
        summary(y)
        
    })
    
    output$scatterplot3 <- renderPlot({
        if (input$variablechoice == "Kobe Bryant") {x <- Kobe[,2]}
        if (input$variablechoice == "Michael Jordan") {x <- MJ[,2]}
        if (input$variablechoice == "Lebron James") {x <- Bron[,2]}
        if (input$variablechoice == "Kevin Durant") {x <- KD[,2]}
        if (input$variablechoice == "Kareem Abdul-Jabbar") {x <- Kareem[,2]}
        if (input$variablechoice == "Magic Johnson") {x <- Magic[,2]}
        if (input$variablechoice == "Larry Bird") {x <- Larry[,2]}
        if (input$variablechoice == "Steph Curry") {x <- Curry[,2]}
        if (input$variablechoice == "Russell Westbrook") {x <- Westbrook[,2]}
        if (input$variablechoice == "Karl Malone") {x <- Karl[,2]}
        
        if (input$variablechoice == "Kobe Bryant") {y <- Kobe[,14]}
        if (input$variablechoice == "Michael Jordan") {y <- MJ[,14]}
        if (input$variablechoice == "Lebron James") {y <- Bron[,14]}
        if (input$variablechoice == "Kevin Durant") {y <- KD[,14]}
        if (input$variablechoice == "Kareem Abdul-Jabbar") {y <- Kareem[,14]}
        if (input$variablechoice == "Magic Johnson") {y <- Magic[,14]}
        if (input$variablechoice == "Larry Bird") {y <- Larry[,14]}
        if (input$variablechoice == "Steph Curry") {y <- Curry[,14]}
        if (input$variablechoice == "Russell Westbrook") {y <- Westbrook[,14]}
        if (input$variablechoice == "Karl Malone") {y <- Karl[,14]}
        
        # draw the histogram with the specified number of bins
        # hist(x, breaks = seq(0,40,by=1), col = 'darkgray', border = 'white', xlab = "age", main = "Average Points Per Game")
        plot(x, y, xlab = "Age", ylab = "3-Point Percentage", main = "Average 3-Point Percentage")
    })
    
    output$summaryofvariable3 <- renderPrint({
        if (input$variablechoice == "Kobe Bryant") {y <- Kobe[,14]}
        if (input$variablechoice == "Michael Jordan") {y <- MJ[,14]}
        if (input$variablechoice == "Lebron James") {y <- Bron[,14]}
        if (input$variablechoice == "Kevin Durant") {y <- KD[,14]}
        if (input$variablechoice == "Kareem Abdul-Jabbar") {y <- Kareem[,14]}
        if (input$variablechoice == "Magic Johnson") {y <- Magic[,14]}
        if (input$variablechoice == "Larry Bird") {y <- Larry[,14]}
        if (input$variablechoice == "Steph Curry") {y <- Curry[,14]}
        if (input$variablechoice == "Russell Westbrook") {y <- Westbrook[,14]}
        if (input$variablechoice == "Karl Malone") {y <- Karl[,14]}
        
        summary(y)
        
    })
    output$usefullabel2 <- renderText({
        "Summary of Average Field Goal Percentage"
    })
    output$usefullabel3 <- renderText({
        "Summary of Average 3-Point Percentage"
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
